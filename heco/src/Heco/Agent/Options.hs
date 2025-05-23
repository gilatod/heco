module Heco.Agent.Options where

import Heco.Data.Default ()
import Heco.Effectful.AccountService.Ldap
    ( LdapGroupAttributes(member, LdapGroupAttributes, name),
      LdapGroupMemberIdentification(GroupMemberIdentifiedByDn),
      LdapOps(..),
      LdapUserAttributes(email, LdapUserAttributes, username, nickname),
      Password(Password),
      Dn(Dn),
      Host(Tls) )
import Heco.Effectful.LanguageService.OpenAI (OpenAIOps(..), openaiOps)
import Heco.Effectful.LanguageService.Ollama (OllamaOps(..))
import Heco.Effectful.DatabaseService.Milvus (MilvusOps(..))
import Heco.Effectful.LanguageService (chatOps)
import Heco.Effectful.DatabaseService
    ( SearchOps(rangeFilter, limit, radius) )
import Heco.Effectful.Ego.Heco (HecoOps(..), immanantContentXMLFormatter, hecoMemoryOps, HecoMemoryOps(..))

import Data.Default (Default(..))
import Data.Text qualified as T

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

ollamaOps :: OllamaOps
ollamaOps = OllamaOps
    { url = "http://127.0.0.1:11434"
    , timeout = Nothing }

makeOpenAIOps :: IO OpenAIOps
makeOpenAIOps = do
    token <- readFile "./tokens/ali.txt"
    pure $ (openaiOps "https://dashscope.aliyuncs.com/compatible-mode/v1")
        { token = Just $ T.pack token }

makeHecoOps :: IO HecoOps
makeHecoOps = do
    characterPrompt <- readFile "./prompts/characters/heco.md"
    taskPrompt <- readFile "./prompts/task.md"
    pure HecoOps
        { characterPrompt = T.pack characterPrompt
        , taskPrompt = T.pack taskPrompt
        , chatOps = chatOps "qwen3-30b-a3b"
        , immanantContentFormatter = immanantContentXMLFormatter
        , memoryOps = (hecoMemoryOps "memory" "bge-m3")
            { searchOps = def
                { limit = Just 10
                , radius = Just 0.05
                , rangeFilter = Just 1 } }
        , messageCacheLimit = Just 64 }

milvusOps :: MilvusOps
milvusOps = MilvusOps
    { url = "http://localhost:19530/v2"
    , timeout = Nothing
    , token = Nothing
    , database = Just "heco" }