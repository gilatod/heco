module Heco.Effectful.AuthService.Ldap where

import Heco.Data.AuthError (AuthError(..))
import Heco.Data.LoginConfig (LoginConfig(..))
import Heco.Data.User (User(..), Username (Username))
import Heco.Data.AuthGroup (AuthGroup(..), GroupName (GroupName))
import Heco.Data.Session (Session(..), SessionToken (SessionToken))
import Heco.Effectful.PrivilegeService (runSimplePrivilegeService)
import Heco.Effectful.AuthService (AuthService(..), login)

import Effectful ((:>), Eff, IOE, MonadIO (liftIO), runEff)
import Effectful.Error.Dynamic (Error, throwError, runError)
import Effectful.Dispatch.Dynamic (reinterpret)
import Effectful.Exception (catchIO, SomeException)
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
      ResponseError(ResponseErrorCode) )

import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty(..), (<|))
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Default (def)
import Data.Maybe (listToMaybe)
import Data.LruCache.IO (LruHandle, newLruHandle, cached)
import Data.UUID.V4 (nextRandom)
import Data.Time (getCurrentTime)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.HashMap.Strict (HashMap, fromList)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (mapMaybe)
import Control.Exception (catch)

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

withLdap :: LdapConfig -> (Ldap -> IO (Either AuthError a)) -> IO (Either AuthError a)
withLdap config f = 
    convertError <$>
        with config.host config.port \l ->
            bind l config.domain config.password >> f l
                `catch` \(e :: ResponseError) -> pure . Left . BackendConnectionError $ show e
    where
        convertError = either (Left . UnhandledAuthError . show) id

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

data ServiceState = ServiceState
    { config :: LdapConfig
    , userCache :: LruHandle Text User
    , sessions :: HashMap SessionToken Session
    , usernameSessionsMap :: HashMap Username (HashSet SessionToken) }

getState ::
    (IOE :> es, State (Maybe ServiceState) :> es)
    => LdapConfig
    -> Eff es ServiceState
getState config = stateM \case
    Nothing -> newState >>= ret
    Just s -> ret s
    where
        ret s = pure (s, Just s)
        newState = ServiceState config
            <$> liftIO (newLruHandle config.uesrCacheSize)
            <*> pure HashMap.empty
            <*> pure HashMap.empty

modifyState ::
    (Error AuthError :> es, State (Maybe ServiceState) :> es)
    => (ServiceState -> ServiceState) -> Eff es ()
modifyState f = modifyM \case
    Just s -> pure . Just $ f s
    Nothing -> throwError $ BackendInternalError "empty service state"

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
    case user of
        Nothing -> pure Nothing
        Just es@(SearchEntry (Dn dn) _) -> do
            Just . (Dn dn,) <$> cached state.userCache dn (createUserCache es dn)
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

createSession :: User -> IO Session
createSession user = do
    uuid <- nextRandom
    time <- getCurrentTime
    pure Session
        { user = user
        , token = SessionToken uuid
        , createTime = time
        , updateTime = time }

runLdapAuthService ::
    (IOE :> es, Error AuthError :> es)
    => LdapConfig
    -> Eff (AuthService : es) a
    -> Eff es a
runLdapAuthService config = reinterpret (evalState emptyState) \_ -> \case
    Login loginConfig -> do
        s <- getState config
        r <- adapt $ withLdap config \l ->
            let (userFilter, password) = convertLoginConfig config loginConfig
                backendError = pure . Left . BackendConnectionError
                doCreateSession (dn, user) = 
                    (bind l dn password >> Right <$> createSession user)
                        `catch` \case
                            (ResponseErrorCode _ InvalidCredentials _ _) ->
                                pure $ Left IncorrectPasswordError
                            e -> backendError . show $ e
                        `catch` \(e :: SomeException) ->
                            backendError . show $ e
            in searchUser l s userFilter
                >>= maybe (pure $ Left UnregisteredUserError) doCreateSession 
        case r of
            Left err -> throwError err
            Right session -> do
                modifyState \s -> s
                    { sessions = HashMap.insert session.token session s.sessions
                    , usernameSessionsMap =
                        HashMap.insertWith HashSet.union session.user.username
                            (HashSet.singleton session.token) s.usernameSessionsMap }
                pure session

    Logout token -> undefined
    GetSession token -> undefined
    GetSessions -> undefined
    AddUserToGroup token username groupName -> undefined
    RemoveUserFromGroup token username groupName -> undefined

    where
        emptyState = Nothing :: Maybe ServiceState
        adapt m = liftIO m `catchIO` (pure . Left . UnhandledAuthError. show)
        convertLoginConfig config = \case
            UsernameLoginConfig username pwd ->
                (Attr config.userAttrs.username := encodeUtf8 username, Password $ encodeUtf8 pwd)
            EmailLoginConfig email pwd ->
                (Attr config.userAttrs.email := encodeUtf8 email, Password $ encodeUtf8 pwd)

test :: IO ()
test = do
    res <- runEff . runError . runSimplePrivilegeService groups . runLdapAuthService config $ do
        login $ UsernameLoginConfig "sicusa" "test"
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