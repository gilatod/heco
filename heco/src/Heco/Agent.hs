{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# LANGUAGE DeriveAnyClass #-}

module Heco.Agent (runHecoAgent) where

import Heco.Data.FunctionSchema
    ( FieldDesc, HasDataSchema, RecordDefault )
import Heco.Data.Default ()
import Heco.Data.Aeson (HasAesonOps, AesonDefault(..))
import Heco.Data.LanguageError (LanguageError(..))
import Heco.Data.Portal.Shell (shellPortal)
import Heco.Data.Portal.OneBot (makeOneBotPortal, OneBotOps(..))
import Heco.Data.LanguageToolRegistry qualified as Registry
import Heco.Effectful.DatabaseService
    ( SearchOps(..) )
import Heco.Events.LanguageEvent (LanguageEvent(..))
import Heco.Effectful.Exception (runThrowEither)
import Heco.Effectful.Event (Event, runEvent)
import Heco.Effectful.LanguageService
    ( LanguageService(..), ChatOps(..), chatOps )
import Heco.Effectful.AccountService.Ldap
    ( runLdapAccountServiceEx,
      LdapOps(..),
      LdapGroupAttributes(..),
      LdapUserAttributes(..),
      Host(..),
      Dn(..),
      Password(..),
      LdapGroupMemberIdentification(..) )
import Heco.Effectful.PrivilegeService (runSimplePrivilegeService)
import Heco.Effectful.LanguageService.OpenAI (OpenAIOps(..), runOpenAILanguageService, openaiOps)
import Heco.Effectful.LanguageService.Ollama (OllamaOps(..))
import Heco.Effectful.LanguageToolProvider.Native (runNativeLanguageToolProviderEx, modifyLangaugeToolRegistry)
import Heco.Effectful.DatabaseService.Milvus (runMilvusDatabaseServiceEx, MilvusOps(..))
import Heco.Effectful.InternalTimeStream.RingBuffer (RingBufferOps(..), runRingBufferInternalTimeStreamEx)
import Heco.Effectful.TaskService (runStandardTaskServiceEx)
import Heco.Effectful.Agent.Heco
    ( hecoMemoryOps,
      immanantContentXMLFormatter,
      runHecoAgentEx,
      HecoMemoryOps(searchOps),
      HecoOps(..) )
import Heco.Effectful.PortalService (runStandardPortalService, runPortal_)
import Heco.Agent.LanguageTools.Archives (archivesToolModule, ArchiveToolOps(..))
import Heco.Agent.LanguageTools.Downloader (downloaderToolModule)
import Heco.Agent.AuthGroups (authGroups)

import Effectful (runEff, Eff, IOE, (:>))
import Effectful.Fail (runFailIO)
import Effectful.Concurrent (runConcurrent, threadDelay)
import Effectful.Dispatch.Dynamic (HasCallStack, reinterpret, send)
import Effectful.Error.Dynamic (CallStack, Error, runError)
import Effectful.Labeled (runLabeled, Labeled(Labeled))
import Effectful.Resource (runResource)
import Effectful.Timeout (runTimeout)

import Data.Text (Text)
import Data.Text qualified as T
import Data.Aeson (FromJSON, ToJSON, Value (Bool))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Default (Default(..))

import Control.Monad (forever, void)
import GHC.Generics (Generic)

runCombinedLanguageService ::
    (HasCallStack, IOE :> es)
    => OpenAIOps -> OllamaOps
    -> Eff (LanguageService : Event LanguageEvent : Error LanguageError : es) a
    -> Eff es (Either (CallStack, LanguageError) a)
runCombinedLanguageService openaiOps ollamaOps =
    runError . runEvent . reinterpret evalInternal \_ -> \case
        Chat chatOps messages -> send $ Labeled @"openai" $ Chat chatOps messages
        Embed modelName text -> send $ Labeled @"openai" $ Embed modelName text
        EmbedMany modelName texts -> send $ Labeled @"openai" $ EmbedMany modelName texts
    where
        evalInternal =
            runLabeled @"openai" (runOpenAILanguageService openaiOps)
            -- . runLabeled @"openai" (runOllamaLanguageService ollamaOps)

data Location = Location
    { name :: FieldDesc (Maybe Text) "Name of location, e.g. San Francisco, CA"
    , datetime :: FieldDesc (Maybe Text) "Date or time, e.g. today, tomorrow, 2023-06-29" }
    -- HasAesonOps 为 Location 提供 JSON 配置
    deriving (Generic, HasAesonOps)
    -- 用 HasAesonOps 里指定的配置来实现 JSON 解码 / 编码
    deriving (FromJSON, ToJSON) via (AesonDefault Location)
    -- 用 HasAesonOps 里指定的配置来实现 JSON Schema 自动创建
    deriving HasDataSchema via (RecordDefault Location)

-- adderTool :: LanguageTool es
--     "adder"
--     "Add two numbers together"
--     (ParamDesc "a" Float "First number" -> ParamDesc "b" Float "Second number" -> Ret Float)
-- adderTool = LanguageTool \a b -> pure $ a + b

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
    memorizingPrompt <- readFile "./prompts/memorize.md"
    pure HecoOps
        { characterPrompt = T.pack characterPrompt
        , taskPrompt = T.pack taskPrompt
        , memorizingPrompt = T.pack memorizingPrompt
        , chatOps = (chatOps "qwen3-235b-a22b")
            { extra = KeyMap.fromList
                [("enable_thinking", Bool False)] }
        , immanantContentFormatter = immanantContentXMLFormatter
        , memoryOps = (hecoMemoryOps "memory" "text-embedding-v3")
            { searchOps = def
                { limit = Just 5
                , radius = Just 0.05
                , rangeFilter = Just 1 } }
        , messageCacheLimit = Just 64 }

runHecoAgent :: IO ()
runHecoAgent = do
    hecoOps <- makeHecoOps
    openaiOps <- makeOpenAIOps

    let runHeco = runEff . runFailIO . runConcurrent . runResource. runTimeout
            . runThrowEither . runStandardTaskServiceEx
            . runSimplePrivilegeService authGroups
            . runThrowEither . runCombinedLanguageService openaiOps ollamaOps
            . runThrowEither . runLdapAccountServiceEx LdapOps
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
            . runThrowEither . runMilvusDatabaseServiceEx MilvusOps
                { url = "http://gilatod.local:19530/v2"
                , timeout = Nothing
                , token = Nothing
                , database = Just "heco" }
            . runThrowEither . runRingBufferInternalTimeStreamEx RingBufferOps
                { capacity = 20 }
            . runThrowEither . runNativeLanguageToolProviderEx
            . runThrowEither . runHecoAgentEx hecoOps
            . runStandardPortalService

    void $ runHeco do
        modifyLangaugeToolRegistry
            $ Registry.addModule
                (archivesToolModule ArchiveToolOps
                    { embeddingModel = "text-embedding-v3"
                    , collectionName = "archives" })
            . Registry.addModule
                downloaderToolModule

        runPortal_ shellPortal
        runPortal_ $ makeOneBotPortal def
            { webapiUrl = "http://gilatod.local:3000"
            , websocketHost = "gilatod.local" }

        forever $ threadDelay maxBound
