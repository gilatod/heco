module Heco.Effectful.AccountService.Ldap
    ( -- * Data types
      LdapUserAttributes(..)
    , LdapGroupMemberIdentification(..)
    , LdapGroupAttributes(..)
    , LdapOps(..)
      -- * Account API
    , runLdapAccountService
    , runLdapAccountServiceEx
      -- * Re-exports
    , Host(..)
    , PortNumber
    , Dn(..)
    , Password(..)
    , AttrValue
    ) where

import Heco.Data.AccountError (AccountError(..))
import Heco.Data.User (User(..), Username(Username))
import Heco.Data.AuthGroup (GroupName(GroupName))
import Heco.Data.Session (SessionToken, Session(..), newSessionToken)
import Heco.Events.AccountEvent (AccountEvent(..))
import Heco.Effectful.AccountService (AccountService(..), LoginOps(..))
import Heco.Effectful.Event (Event, trigger, runEvent)

import Effectful ((:>), Eff, IOE, MonadIO(liftIO))
import Effectful.Error.Dynamic (Error, throwError, runError, CallStack)
import Effectful.Dispatch.Dynamic (reinterpret, HasCallStack)
import Effectful.State.Static.Shared (State, evalState, modify, get)
import Effectful.Exception (SomeException)

import Ldap.Client
    ( Host,
      PortNumber,
      Dn(..),
      Password(..),
      AttrValue,
      Ldap,
      Filter((:=), And),
      SearchEntry(..),
      Attr(Attr),
      ResultCode(InvalidCredentials),
      ResponseError(ResponseErrorCode), Operation (Replace) )
import Ldap.Client qualified as Ldap

import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty(..), (<|))
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Maybe ( listToMaybe, mapMaybe )
import Data.Time (getCurrentTime)
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.HashMap.Strict (HashMap, fromList)
import Data.HashMap.Strict qualified as HashMap
import Data.Function ((&))
import Control.Exception (catch, throw)

data LdapUserAttributes = LdapUserAttributes
    { username :: Text
    , nickname :: Text
    , email :: Text }
    deriving (Eq, Show)

data LdapGroupMemberIdentification
    = GroupMemberIdentifiedByUsername
    | GroupMemberIdentifiedByDn
    deriving (Eq, Enum, Show)

data LdapGroupAttributes = LdapGroupAttributes
    { name :: Text
    , member :: Text }
    deriving (Eq, Show)

data LdapOps = LdapOps
    { host :: Host
    , port :: PortNumber
    , domain :: Dn
    , password :: Password
    , userBase :: Dn
    , userObjectClass :: AttrValue
    , userExtra :: [(Text, AttrValue)]
    , userAttrs :: LdapUserAttributes
    , groupBase :: Dn
    , groupObjectClass :: AttrValue
    , groupExtra :: [(Text, AttrValue)]
    , groupAttrs :: LdapGroupAttributes
    , groupMemberIdentification :: LdapGroupMemberIdentification }
    deriving (Show)

type LdapAttrMap = HashMap Text [Text]

withLdap :: HasCallStack => LdapOps -> (Ldap -> IO a) -> IO a
withLdap ops f =
    either throwUnhandledError id <$>
        Ldap.with ops.host ops.port \l -> do
            f l `catch` \(e :: ResponseError) ->
                throw . AccountBackendError . show $ e
    where
        throwUnhandledError = throw . UnhandledAccountError . show

withLdapDefaultBind :: HasCallStack => LdapOps -> (Ldap -> IO a) -> IO a
withLdapDefaultBind ops f = withLdap ops \l ->
    Ldap.bind l ops.domain ops.password >> f l

searchEntries :: Ldap -> Dn -> AttrValue -> [(Text, AttrValue)] -> Filter -> IO [SearchEntry]
searchEntries ldap base objectClass extra filter =
    Ldap.search ldap base mempty
        (And $ Attr "objectClass" := objectClass
            <| filter :| map (\(k, v) -> Attr k := v) extra) []

searchEntry :: Ldap -> Dn -> AttrValue -> [(Text, AttrValue)] -> Filter -> IO (Maybe SearchEntry)
searchEntry ldap base objectClass extra filter =
    listToMaybe <$> searchEntries ldap base objectClass extra filter

entryToMap :: SearchEntry -> LdapAttrMap
entryToMap (SearchEntry _ attrList) =
    fromList $ map convertPair attrList
    where
        convertPair (Attr attr, values) = (attr, map decodeUtf8 values)

data ActiveUser = ActiveUser
    { user :: User
    , tokens :: HashSet SessionToken
    , dn :: Dn
    , password :: Password }

data ServiceState = ServiceState
    { ops :: LdapOps
    , sessions :: HashMap SessionToken Session
    , activeUsers :: HashMap Username ActiveUser }

getActiveUser ::
    Error AccountError :> es
    => ServiceState -> Username -> Eff es ActiveUser
getActiveUser serviceState username =
    maybe (throwError UnregisteredUserError) pure $
        HashMap.lookup username serviceState.activeUsers

modifyActiveUser ::
    State ServiceState :> es
    => Username -> (ActiveUser -> ActiveUser) -> Eff es ()
modifyActiveUser username f = modify \s ->
    let (maybeRenamedUser, activeUsers') =
            HashMap.alterF alterActiveUser username s.activeUsers
        activeUsers'' =
            case maybeRenamedUser of
                Just au -> HashMap.insert au.user.username au activeUsers'
                Nothing -> activeUsers'
    in s { activeUsers = activeUsers'' }
    where
        alterActiveUser (Just au) =
            let au' = f au
            in if au'.user.username == username
                then (Nothing, Just au')
                else (Just au', Nothing)
        alterActiveUser Nothing = (Nothing, Nothing)

registerSession ::
    State ServiceState :> es
    => User -> Dn -> Password -> Session -> Eff es Session
registerSession user dn password session = do
    modify \s -> s
        { sessions = HashMap.insert session.token session s.sessions
        , activeUsers = HashMap.alter alterActiveUser session.username s.activeUsers }
    pure session
    where
        alterActiveUser = \case
            Just au -> Just au
                { tokens = HashSet.insert session.token au.tokens }
            Nothing -> Just ActiveUser
                { user = user
                , dn = dn
                , tokens = HashSet.singleton session.token
                , password = password }

unregisterSession ::
    State ServiceState :> es
    => Session -> Eff es Session
unregisterSession session = do
    modify \s -> s
        { sessions = HashMap.delete session.token s.sessions
        , activeUsers = HashMap.update
            updateActiveUser session.username s.activeUsers }
    pure session
    where
        updateActiveUser ud = Just ud
            { tokens = HashSet.delete session.token ud.tokens }

requireSessionAndState ::
    (Error AccountError :> es, State ServiceState :> es)
    => SessionToken -> Eff es (Session, ServiceState)
requireSessionAndState token = get >>= \s ->
    case HashMap.lookup token s.sessions of
        Just session -> pure (session, s)
        Nothing -> throwError InvalidSessionTokenError

requireSession ::
    (Error AccountError :> es, State ServiceState :> es)
    => SessionToken -> Eff es Session
requireSession = fmap fst . requireSessionAndState

searchUserGroups :: Ldap -> ServiceState -> Filter -> IO [GroupName]
searchUserGroups ldap state filter = do
    let ops = state.ops
    groups <- searchEntries ldap
        ops.groupBase
        ops.groupObjectClass
        ops.groupExtra
        filter
    pure $ groups & mapMaybe \(SearchEntry _ entries) ->
        case lookup (Attr ops.groupAttrs.name) entries of
            Nothing -> Nothing
            Just [] -> Nothing
            Just (v:_) -> Just $ GroupName (decodeUtf8 v)

searchUser :: Ldap -> ServiceState -> Filter -> IO (Maybe (Dn, User))
searchUser ldap state filter = do
    user <- searchEntry ldap
        ops.userBase
        ops.userObjectClass
        ops.userExtra
        filter
    case user of
        Nothing -> pure Nothing
        Just es@(SearchEntry (Dn dn) _) ->
            Just . (Dn dn,) <$> createUserRecord es dn
    where
        ops = state.ops
        createUserRecord es dn = do
            let attrs = ops.userAttrs
                attrMap = entryToMap es
                getAttr k defaultValue =
                    case HashMap.lookup k attrMap of
                        Nothing -> defaultValue
                        Just [] -> defaultValue
                        Just (v:_) -> v
                username = getAttr attrs.username "[unknown]"
            groups <- searchUserGroups ldap state $
                Attr ops.groupAttrs.member :=
                    case ops.groupMemberIdentification of
                        GroupMemberIdentifiedByDn -> encodeUtf8 dn
                        GroupMemberIdentifiedByUsername -> encodeUtf8 username
            pure User
                { username = Username username
                , nickname = getAttr attrs.nickname ""
                , email = getAttr attrs.email ""
                , groups = groups }

bindUser :: HasCallStack => Ldap -> Dn -> Password -> IO ()
bindUser ldap dn password = Ldap.bind ldap dn password
    `catch` \case
        (ResponseErrorCode _ InvalidCredentials _ _) ->
            throw IncorrectPasswordError
        e -> throw . AccountBackendError . show $ e

adapt :: (HasCallStack, Error AccountError :> es, IOE :> es) => IO b -> Eff es b
adapt m = do
    r <- liftIO $ (Right <$> m)
        `catch` \(e :: AccountError) ->
            pure (Left e)
        `catch` \(e :: SomeException) ->
            pure (Left . UnhandledAccountError . show $ e)
    either throwError pure r

data UserOperationContext = UserOperationContext
    { serviceState :: ServiceState
    , activeUser :: ActiveUser
    , session :: Session }

withUser ::
    (HasCallStack, IOE :> es, Error AccountError :> es, State ServiceState :> es)
    => LdapOps -> SessionToken
    -> (Ldap -> UserOperationContext -> IO a)
    -> Eff es a
withUser ops token f = do
    (session, state) <- requireSessionAndState token
    ud <- getActiveUser state session.username
    adapt $ withLdap ops \l -> do
        bindUser l ud.dn ud.password
        f l UserOperationContext
            { serviceState = state
            , activeUser = ud
            , session = session }

convertLoginOps :: LdapOps -> LoginOps -> (Filter, Password)
convertLoginOps ops = \case
    UsernameLoginOps username pwd ->
        (Attr ops.userAttrs.username := encodeUtf8 username, Password $ encodeUtf8 pwd)
    EmailLoginOps email pwd ->
        (Attr ops.userAttrs.email := encodeUtf8 email, Password $ encodeUtf8 pwd)

runLdapAccountService ::
    (HasCallStack, IOE :> es, Event AccountEvent :> es, Error AccountError :> es)
    => LdapOps
    -> Eff (AccountService : es) a
    -> Eff es a
runLdapAccountService ops = reinterpret wrap \_ -> \case
    Login loginOps -> do
        s <- get
        (dn, user, password) <- adapt $ withLdapDefaultBind ops \l ->
            let (userFilter, password) = convertLoginOps ops loginOps
            in searchUser l s userFilter
                >>= maybe (throw UnregisteredUserError) \(dn, user) -> do
                    bindUser l dn password
                    pure (dn, user, password)

        token <- liftIO newSessionToken
        time <- liftIO getCurrentTime
        session <- registerSession user dn password Session
            { username = user.username
            , token = token
            , createTime = time }

        trigger $ OnAccountLogin session
        pure token

    Logout token -> do
        session <- requireSession token >>= unregisterSession
        trigger $ OnAccountLogout session

    GetSessions ->
        get @ServiceState >>= \s -> pure . HashMap.elems $ s.sessions
    GetSession token ->
        requireSession token
    GetUser token -> do
        (session, state) <- requireSessionAndState token
        case HashMap.lookup session.username state.activeUsers of
            Just au -> pure au.user
            Nothing -> throwError $ BackendInternalError "user not found"

    SetUsername token name'@(Username nameText') -> do
        (dn', prevUsername, session) <- withUser ops token \l ctx -> do
            let ud = ctx.activeUser
                userDn = ud.dn
                name@(Username nameText) = ctx.session.username

            bindUser l userDn ud.password
            Ldap.modify l userDn
                [Replace (Attr ops.userAttrs.username) [encodeUtf8 nameText']]

            Ldap.bind l ops.domain ops.password
            searchUser l ctx.serviceState
                (Attr ops.userAttrs.username := encodeUtf8 nameText)
                >>= maybe (throw $ BackendInternalError "user not found")
                    (pure . (, name, ctx.session) . fst)

        modifyActiveUser prevUsername \au -> au
            { user = au.user { username = name' }
            , dn = dn' }

        trigger $ OnAccountUsernameChanged session name'

    SetNickname token nickname -> do
        session <- withUser ops token \l ctx -> do
            Ldap.modify l ctx.activeUser.dn
                [Replace (Attr ops.userAttrs.nickname) [encodeUtf8 nickname]]
            pure ctx.session

        modifyActiveUser session.username \au -> au
            { user = au.user { nickname = nickname } }

        trigger $ OnAccountNicknameChanged session nickname

    SetEmail token email -> do
        session <- withUser ops token \l ctx -> do
            Ldap.modify l ctx.activeUser.dn
                [Replace (Attr ops.userAttrs.email) [encodeUtf8 email]]
            pure ctx.session

        modifyActiveUser session.username \au -> au
            { user = au.user { email = email } }

        trigger $ OnAccountEmailChanged session email

    where
        wrap e = do
            let state = ServiceState
                    { ops = ops
                    , sessions = HashMap.empty
                    , activeUsers = HashMap.empty }
            evalState state e

runLdapAccountServiceEx ::
    (HasCallStack, IOE :> es)
    => LdapOps
    -> Eff (AccountService : Event AccountEvent : Error AccountError : es) a
    -> Eff es (Either (CallStack, AccountError) a)
runLdapAccountServiceEx ops =
    runError . runEvent . runLdapAccountService ops