module Main (main) where

import Heco.Data.AuthGroup (AuthGroup(..))
import Heco.Data.Session (Session(..), SessionToken (SessionToken))
import Heco.Data.Role (Role(..))
import Heco.Data.Message (Message(..))
import Heco.Events.LanguageEvent (LanguageEvent(..))
import Heco.Effectful.LanguageService (chat, embed)
import Heco.Effectful.AccountService (LoginOps(..), login, getUser, logout)
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
import Heco.Effectful.SessionContext (runSessionContext, getSessionContext)
import Heco.Effectful.LanguageService.Ollama (runOllamaLanguageServiceEx, defaultOllamaOps, OllamaOps(..), OllamaModels(..))

import Effectful (runEff, liftIO)

import Data.Default (Default(..))
import Data.HashSet qualified as HashSet
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.List.NonEmpty (NonEmpty(..))
import Data.UUID.V4 (nextRandom)
import Heco.Effectful.Event (listen_)
import GHC.IO.Handle (hFlush)
import System.IO (stdout)
import Control.Monad (when)

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
    , uesrCacheSize = 50
    , groupBase = Dn "ou=groups,dc=gilatod,dc=art"
    , groupObjectClass = "posixGroup"
    , groupExtra = []
    , groupAttrs = LdapGroupAttributes
        { name = "cn"
        , member = "uniqueMember" }
    , groupMemberIdentification = GroupMemberIdentifiedByDn }

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

testLdap :: IO ()
testLdap = do
    let run = runEff
            . runSimplePrivilegeService groups
            . runLdapAccountServiceEx ldapOps
            . runSessionContext @Int
                (\s -> liftIO (putStrLn $ "Logged in: " ++ show s.username) >> pure 0)
                (\s _ -> liftIO (putStrLn $ "Logged out: " ++ show s.username))
    res <- run do
        token <- login $ UsernameLoginOps "test" "Holders-instance-14-sulfur"
        user <- getUser token
        ctx <- getSessionContext token
        liftIO $ putStrLn $ show ctx
        logout token
        pure user
    putStrLn $ show res
    where

testOllama :: IO ()
testOllama = do
    let run = runEff
            . runSimplePrivilegeService groups
            . runLdapAccountServiceEx ldapOps
            . runOllamaLanguageServiceEx ops
    _ <- run do
        token <- SessionToken <$> liftIO nextRandom
        prompt <- Message System <$> (liftIO . fmap T.pack . readFile $ "prompt.txt")
        listen_ \case
            OnLanguageChunkReceived _ msg -> liftIO do
                T.putStr $ msg.content
                hFlush stdout
            OnLanguageResponseReceived _ _ -> liftIO $ putStr "\n"
            _ -> pure ()
        res <- chat token $ prompt :| [Message User "你的名字叫什么？"]
        embeddings <- embed token "你好世界"
        liftIO $ putStrLn $ show embeddings
    pure ()
    where
        ops = defaultOllamaOps
            { models = OllamaModels
                { chat = "deepseek-r1:14b"
                , embeddings = "mxbai-embed-large:latest" } }

main :: IO ()
main = do
    testOllama