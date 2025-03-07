{-# LANGUAGE DeriveAnyClass #-}

module Main (main) where

import Heco.Data.Default ()
import Heco.Data.AuthGroup (AuthGroup(..))
import Heco.Data.Session (Session(..))
import Heco.Data.Role (Role(..))
import Heco.Data.Message (Message(..))
import Heco.Data.Embedding (Embedding(Embedding))
import Heco.Data.Entity (EntityId)
import Heco.Data.Entity.TH (deriveEntity)
import Heco.Events.LanguageEvent (LanguageEvent(..))
import Heco.Effectful.Exception (eitherThrowIO)
import Heco.Effectful.Event (on, Event)
import Heco.Effectful.LanguageService (embed, chat_, LanguageService)
import Heco.Effectful.AccountService (LoginOps(..), login, getUser, logout, AccountService)
import Heco.Effectful.AccountService.Ldap
    ( LdapOps(..),
      Host(Tls),
      Dn(Dn),
      Password(Password),
      LdapUserAttributes(..),
      LdapGroupAttributes(..),
      LdapGroupMemberIdentification(..),
      runLdapAccountServiceEx )
import Heco.Effectful.PrivilegeService (runSimplePrivilegeService)
import Heco.Effectful.SessionContext (runSessionContext, getSessionContext, SessionContext)
import Heco.Effectful.LanguageService.Ollama (runOllamaLanguageServiceEx)
import Heco.Effectful.DatabaseService.Milvus (runMilvusDatabaseServiceEx, MilvusOps(..))
import Heco.Effectful.DatabaseService
    ( DatabaseService,
      addEntity,
      getEntities,
      loadCollection, setEntity_ )

import Effectful (runEff, liftIO, Eff, IOE, (:>))
import Effectful.Fail (Fail, runFailIO)

import Data.Default (Default(..))
import Data.HashSet qualified as HashSet
import Data.List.NonEmpty (appendList, singleton)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Vector.Unboxing qualified as VU
import GHC.IO.Handle (hFlush)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import System.IO (stdout)

ldapOps :: LdapOps
ldapOps = LdapOps
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
    , groupBase = Dn "ou=groups,dc=gilatod,dc=art"
    , groupObjectClass = "posixGroup"
    , groupExtra = []
    , groupAttrs = LdapGroupAttributes
        { name = "cn"
        , member = "uniqueMember" }
    , groupMemberIdentification = GroupMemberIdentifiedByDn }

milvusOps :: MilvusOps
milvusOps = MilvusOps
    { hostUrl = "http://localhost:19530"
    , timeout = 15
    , token = Nothing
    , database = Just "heco" }

groups :: [AuthGroup]
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

testLdap ::
    ( HasCallStack
    , AccountService :> es
    , SessionContext Int :> es
    , IOE :> es )
    => Eff es ()
testLdap = do
    token <- login $ UsernameLoginOps "test" "Holders-instance-14-sulfur"
    user <- getUser token
    liftIO . putStrLn . show $ user
    ctx <- getSessionContext token
    liftIO $ putStrLn $ show ctx
    logout token
    pure ()

testOllama ::
    ( HasCallStack
    , LanguageService :> es
    , Event LanguageEvent :> es
    , AccountService :> es
    , IOE :> es )
    => Eff es ()
testOllama = do
    token <- login $ UsernameLoginOps "test" "Holders-instance-14-sulfur"
    prompt <- Message System <$> (liftIO . fmap T.pack . readFile $ "prompt copy.txt")
    doChat token (singleton prompt)
        `on` \case
            OnLanguageChunkReceived _ msg -> liftIO do
                T.putStr $ msg.content
                hFlush stdout
            OnLanguageResponseReceived _ _ -> liftIO $ putStr "\n"
            _ -> pure ()
    where
        doChat token messages = do
            liftIO $ putStr "> " >> hFlush stdout
            input <- liftIO $ getLine
            let messages' = appendList messages [Message User $ T.pack input]
            chat_ token chatModel messages'
            doChat token messages'
        chatModel = "fairy:14b"

data TestEntity = TestEntity
    { id :: Maybe EntityId
    , vector :: Maybe (VU.Vector Float)
    , text :: Text }
    deriving (Show, Generic, Default)

deriveEntity ''TestEntity

testMilvus ::
    ( HasCallStack
    , LanguageService :> es
    , DatabaseService :> es
    , AccountService :> es
    , Fail :> es, IOE :> es )
    => Eff es ()
testMilvus = do
    token <- login $ UsernameLoginOps "test" "Holders-instance-14-sulfur"

    [Embedding vector] <- embed token "mxbai-embed-large" ["你好世界"]
    liftIO . putStrLn . show $ VU.length vector

    loadCollection "main"

    eid <- addEntity "main" TestEntity
        { id = Nothing
        , vector = Just vector
        , text = "hello world" }

    eid2 <- addEntity "main" TestEntity
        { id = Nothing
        , vector = Just vector
        , text = "hello world xxxx" }

    setEntity_ "main" TestEntity
        { id = Just eid
        , vector = Just vector
        , text = "hello world 2" }

    res <- getEntities @TestEntity "main" $ VU.fromList [eid, eid2]
    liftIO $ putStrLn $ show res
    pure()

main :: IO ()
main = do
    let run = runEff
            . runFailIO
            . eitherThrowIO . runLdapAccountServiceEx ldapOps
            . eitherThrowIO . runMilvusDatabaseServiceEx milvusOps
            . runSimplePrivilegeService groups
            . runOllamaLanguageServiceEx def
            . runSessionContext @Int
                (\s -> liftIO (putStrLn $ "Logged in: " ++ show s.username) >> pure 0)
                (\s _ -> liftIO (putStrLn $ "Logged out: " ++ show s.username))
    _ <- run testMilvus
    pure ()