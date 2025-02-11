module Heco.Effectful.AccountService.Ldap where

import Heco.Data.AccountError (AccountError(..))
import Heco.Data.LoginConfig (LoginConfig(..))
import Heco.Data.User (User(..), Username (Username))
import Heco.Data.AuthGroup (AuthGroup(..), GroupName (GroupName))
import Heco.Data.Session (Session(..), SessionToken (SessionToken))
import Heco.Effectful.PrivilegeService (runSimplePrivilegeService)
import Heco.Effectful.AccountService (AccountService(..), login, getSession, setNickname)

import Effectful ((:>), Eff, IOE, MonadIO (liftIO), runEff)
import Effectful.Error.Dynamic (Error, throwError, runError)
import Effectful.Dispatch.Dynamic (reinterpret)
import Effectful.State.Static.Shared (State, evalState, modifyM, stateM)

import Ldap.Client
    ( Host(Tls),
      PortNumber,
      Dn(..),
      Password(..),
      AttrValue,
      Ldap,
      LdapError,
      Filter((:=), And),
      SearchEntry(..),
      Attr(Attr),
      with,
      bind,
      search,
      ResultCode(InvalidCredentials),
      ResponseError(ResponseErrorCode), Operation (Replace), modify )

import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty(..), (<|))
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Default (def)
import Data.Maybe (listToMaybe)
import Data.LruCache.IO (StripedLruHandle, newStripedLruHandle, stripedCached)
import Data.UUID.V4 (nextRandom)
import Data.Time (getCurrentTime)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.HashMap.Strict (HashMap, fromList)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (mapMaybe)
import Control.Exception (catch, throw)
import Control.Arrow (Arrow(second))
import Effectful.Exception (SomeException)

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

data LdapConfig = LdapConfig
    { host :: Host
    , port :: PortNumber
    , domain :: Dn
    , password :: Password
    , userBase :: Dn
    , userObjectClass :: AttrValue
    , userExtra :: [(Text, AttrValue)]
    , userAttrs :: LdapUserAttributes
    , uesrCacheSize :: Int
    , groupBase :: Dn
    , groupObjectClass :: AttrValue
    , groupExtra :: [(Text, AttrValue)]
    , groupAttrs :: LdapGroupAttributes
    , groupMemberIdentification :: LdapGroupMemberIdentification }
    deriving (Show)

type LdapResult a = IO (Either LdapError a)
type LdapAttrMap = HashMap Text [Text]

withLdap :: LdapConfig -> (Ldap -> IO a) -> IO a
withLdap config f = 
    either throwUnhandledError id <$>
        with config.host config.port \l -> do
            f l `catch` \(e :: ResponseError) ->
                throw . BackendOperationError . show $ e
    where
        throwUnhandledError = throw . UnhandledAccountError . show

withLdapDefaultBind :: LdapConfig -> (Ldap -> IO a) -> IO a
withLdapDefaultBind config f = withLdap config \l ->
    bind l config.domain config.password >> f l

searchEntries :: Ldap -> Dn -> AttrValue -> [(Text, AttrValue)] -> Filter -> IO [SearchEntry]
searchEntries ldap base objectClass extra filter =
    search ldap base mempty
        (And $ Attr "objectClass" := objectClass
            <| filter :| map (\(k, v) -> Attr k := v) extra) []

searchEntry :: Ldap -> Dn -> AttrValue -> [(Text, AttrValue)] -> Filter -> IO (Maybe SearchEntry)
searchEntry ldap base objectClass extra filter =
    listToMaybe <$> searchEntries ldap base objectClass extra filter

entryToMap :: SearchEntry -> LdapAttrMap
entryToMap (SearchEntry _ attrList) =
    fromList $ map convertPair attrList
    where
        convertPair ((Attr attr), values) = (attr, map decodeUtf8 values)

data UserDetail = UserDetail
    { tokens :: HashSet SessionToken
    , dn :: Dn
    , password :: Password }

data ServiceState = ServiceState
    { config :: LdapConfig
    , sessions :: HashMap SessionToken Session
    , userDetails :: HashMap Username UserDetail
    , userCache :: StripedLruHandle Text User }

type ServiceStateEff = State (Maybe ServiceState)

getState ::
    (IOE :> es, ServiceStateEff :> es)
    => LdapConfig
    -> Eff es ServiceState
getState config = stateM \case
    Nothing -> newState >>= ret
    Just s -> ret s
    where
        ret s = pure (s, Just s)
        stripCount = 8
        newState = ServiceState config HashMap.empty HashMap.empty
            <$> liftIO (newStripedLruHandle stripCount (config.uesrCacheSize `div` stripCount))

modifyServiceState ::
    (Error AccountError :> es, ServiceStateEff :> es)
    => (ServiceState -> Eff es ServiceState) -> Eff es ()
modifyServiceState f = modifyM \case
    Just s -> Just <$> f s
    Nothing -> throwError $ BackendInternalError "empty service state"

serviceState ::
    (Error AccountError :> es, ServiceStateEff :> es)
    => (ServiceState -> Eff es (a, ServiceState)) -> Eff es a
serviceState f = stateM \case
    Just s -> second Just <$> f s
    Nothing -> throwError $ BackendInternalError "empty service state"

getUserDetail ::
    Error AccountError :> es
    => ServiceState -> Username -> Eff es UserDetail
getUserDetail serviceState username =
    maybe (throwError UnregisteredUserError) pure $
        HashMap.lookup username serviceState.userDetails

registerSession ::
    (Error AccountError :> es, ServiceStateEff :> es)
    => Dn -> Password -> Session -> Eff es ()
registerSession dn password session =
    modifyServiceState \s -> pure s
        { sessions = HashMap.insert session.token session s.sessions
        , userDetails = HashMap.alter alterUserDetail session.user.username s.userDetails }
    where
        alterUserDetail = \case
            Just ud -> Just ud
                { tokens = HashSet.insert session.token ud.tokens }
            Nothing -> Just UserDetail
                { dn = dn
                , tokens = HashSet.singleton session.token
                , password = password }

unregisterSession ::
    (Error AccountError :> es, ServiceStateEff :> es)
    => Session -> Eff es ()
unregisterSession session =
    modifyServiceState \s -> pure s
        { sessions = HashMap.delete session.token s.sessions
        , userDetails = HashMap.update
            updateUserDetail session.user.username s.userDetails }
    where
        updateUserDetail ud = Just ud
            { tokens = HashSet.delete session.token ud.tokens }

requireSessionAndState ::
    (IOE :> es, Error AccountError :> es, ServiceStateEff :> es)
    => SessionToken -> Eff es (Session, ServiceState)
requireSessionAndState token = serviceState \s ->
    case HashMap.lookup token s.sessions of
        Just session -> do
            now <- liftIO $ getCurrentTime
            let session' = session { updateTime = now }
                sessions' = HashMap.insert session.token session' s.sessions
                s' = s { sessions = sessions' }
            pure ((session', s'), s')
        Nothing -> throwError InvalidSessionToken
 
requireSession ::
    (IOE :> es, Error AccountError :> es, ServiceStateEff :> es)
    => SessionToken -> Eff es Session
requireSession = fmap fst . requireSessionAndState

searchUserGroups :: Ldap -> ServiceState -> Filter -> IO [GroupName]
searchUserGroups ldap state filter = do
    let config = state.config
    groups <- searchEntries ldap
        config.groupBase
        config.groupObjectClass
        config.groupExtra
        filter
    pure $ flip mapMaybe groups \(SearchEntry _ entries) ->
        case lookup (Attr config.groupAttrs.name) entries of
            Nothing -> Nothing
            Just [] -> Nothing
            Just (v:_) -> Just $ GroupName (decodeUtf8 v)

searchUser :: Ldap -> ServiceState -> Filter -> IO (Maybe (Dn, User))
searchUser ldap state filter = do
    user <- searchEntry ldap
        config.userBase
        config.userObjectClass
        config.userExtra
        filter
    putStrLn $ show user
    case user of
        Nothing -> pure Nothing
        Just es@(SearchEntry (Dn dn) _) ->
            Just . (Dn dn,) <$> stripedCached state.userCache dn (createUserCache es dn)
    where
        config = state.config
        createUserCache es dn = do
            let attrs = config.userAttrs
                attrMap = entryToMap es
                getAttr k defaultValue =
                    case HashMap.lookup k attrMap of
                        Nothing -> defaultValue
                        Just [] -> defaultValue
                        Just (v:_) -> v
                username = getAttr attrs.username "[unknown]"
            groups <- searchUserGroups ldap state $
                Attr config.groupAttrs.member :=
                    case config.groupMemberIdentification of
                        GroupMemberIdentifiedByDn -> encodeUtf8 dn
                        GroupMemberIdentifiedByUsername -> encodeUtf8 username
            pure User
                { username = Username username
                , nickname = getAttr attrs.nickname ""
                , email = getAttr attrs.email ""
                , groups = groups }

bindUser :: Ldap -> Dn -> Password -> IO ()
bindUser ldap dn password = bind ldap dn password
    `catch` \case
        (ResponseErrorCode _ InvalidCredentials _ _) ->
            throw IncorrectPasswordError
        e -> throw . BackendOperationError . show $ e

adapt :: (Error AccountError :> es, IOE :> es) => IO b -> Eff es b
adapt m = do
    r <- liftIO $ (Right <$> m)
        `catch` \(e :: AccountError) ->
            pure (Left e)
        `catch` \(e :: SomeException) ->
            pure (Left . UnhandledAccountError . show $ e)
    either throwError pure r

data UserOperationContext = UserOperationContext
    { serviceState :: ServiceState
    , userDetail :: UserDetail
    , session :: Session }

withUser ::
    (IOE :> es, Error AccountError :> es, ServiceStateEff :> es)
    => LdapConfig -> SessionToken
    -> (Ldap -> UserOperationContext -> IO a)
    -> Eff es a
withUser config token f = do
    (session, state) <- requireSessionAndState token
    ud <- getUserDetail state session.user.username
    adapt $ withLdap config \l -> do
        bindUser l ud.dn ud.password
        f l UserOperationContext
            { serviceState = state
            , userDetail = ud
            , session = session }

convertLoginConfig :: LdapConfig -> LoginConfig -> (Filter, Password)
convertLoginConfig config = \case
    UsernameLoginConfig username pwd ->
        (Attr config.userAttrs.username := encodeUtf8 username, Password $ encodeUtf8 pwd)
    EmailLoginConfig email pwd ->
        (Attr config.userAttrs.email := encodeUtf8 email, Password $ encodeUtf8 pwd)

runLdapAccountService ::
    (IOE :> es, Error AccountError :> es)
    => LdapConfig
    -> Eff (AccountService : es) a
    -> Eff es a
runLdapAccountService config = reinterpret (evalState emptyState) \_ -> \case
    Login loginConfig -> do
        s <- getState config

        (dn, user, password) <- adapt $ withLdapDefaultBind config \l ->
            let (userFilter, password) = convertLoginConfig config loginConfig
            in searchUser l s userFilter
                >>= maybe (throw UnregisteredUserError) \(dn, user) -> do
                    bindUser l dn password
                    pure (dn, user, password)

        token <- SessionToken <$> liftIO nextRandom
        time <- liftIO getCurrentTime

        registerSession dn password Session
            { user = user
            , token = token
            , createTime = time
            , updateTime = time }
        pure token

    Logout token ->
        requireSession token >>= unregisterSession

    GetSession token ->
        requireSession token
    GetSessions ->
        getState config >>= \s -> pure . HashMap.elems $ s.sessions

    SetUsername token newName@(Username newNameText) -> do
        (newDn, prevUsername) <- withUser config token \l ctx -> do
            let ud = ctx.userDetail
                userDn = ud.dn
                username@(Username usernameText) = ctx.session.user.username

            bindUser l userDn ud.password
            modify l userDn
                [Replace (Attr config.userAttrs.username) [encodeUtf8 newNameText]]

            bind l config.domain config.password
            searchUser l ctx.serviceState
                (Attr config.userAttrs.username := encodeUtf8 usernameText)
                >>= maybe (throw $ BackendInternalError "user not found")
                    (pure . (,username) . fst)

        modifyServiceState \s -> do
            let (maybeUs, userDetails') =
                    HashMap.alterF (\x -> (x, Nothing)) prevUsername s.userDetails
                userDetails'' = 
                    case maybeUs of
                        Just ud -> HashMap.insert newName (ud { dn = newDn }) userDetails'
                        Nothing -> s.userDetails
            pure s { userDetails = userDetails'' }
             
    SetNickname token nickname ->
        withUser config token \l ctx -> do
            modify l ctx.userDetail.dn
                [Replace (Attr config.userAttrs.nickname) [encodeUtf8 nickname]]

    SetEmail token email ->
        withUser config token \l ctx -> do
            modify l ctx.userDetail.dn
                [Replace (Attr config.userAttrs.email) [encodeUtf8 email]]

    where
        emptyState = Nothing :: Maybe ServiceState

test :: IO ()
test = do
    let run = runEff
            . runError
            . runSimplePrivilegeService groups
            . runLdapAccountService config
    res <- run do
        token <- login $ UsernameLoginConfig "test" "Holders-instance-14-sulfur"
        setNickname token "newName"
        getSession token
        --logout session.token
    putStrLn $ show res
    pure ()
    where
        groups =
            [ AuthGroup
                { name = "admin"
                , description = "Administrators"
                , privileges = HashSet.empty }
            , AuthGroup
                { name = "editor"
                , description = "Editors"
                , privileges = HashSet.empty }
            , AuthGroup
                { name = "author"
                , description = "Authors"
                , privileges = HashSet.empty }
            , AuthGroup
                { name = "blocked"
                , description = "Blocked users"
                , privileges = HashSet.empty } ]
        config = LdapConfig
            { host = Tls "auth.gilatod.art" def
            , port = 636
            , domain = Dn "cn=readonly,dc=gilatod,dc=art"
            , password = Password "readonly"
            , userBase = Dn "ou=people,dc=gilatod,dc=art"
            , userObjectClass = "posixAccount"
            , userExtra = []
            , userAttrs = LdapUserAttributes
                { username = "uid"
                , nickname = "displayName"
                , email = "mail" }
            , uesrCacheSize = 50
            , groupBase = Dn "ou=groups,dc=gilatod,dc=art"
            , groupObjectClass = "posixGroup"
            , groupExtra = []
            , groupAttrs = LdapGroupAttributes
                { name = "cn"
                , member = "uniqueMember" }
            , groupMemberIdentification = GroupMemberIdentifiedByDn }