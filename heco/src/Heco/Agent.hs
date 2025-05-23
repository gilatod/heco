{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# LANGUAGE DeriveAnyClass #-}

module Heco.Agent (runHecoAgent) where

import Heco.Data.FunctionSchema
    ( FieldDesc, HasDataSchema, RecordDefault )
import Heco.Data.Default ()
import Heco.Data.Aeson (HasAesonOps, AesonDefault(..))
import Heco.Data.LanguageTool (LanguageTool(..), Param, Ret)
import Heco.Data.LanguageError (LanguageError(..))
import Heco.Data.Portal.Shell (shellPortal)
import Heco.Data.Portal.OneBot (makeOneBotPortal, OneBotOps(..))
import Heco.Events.LanguageEvent (LanguageEvent(..))
import Heco.Effectful.Exception (runThrowEither)
import Heco.Effectful.Event (Event, runEvent)
import Heco.Effectful.LanguageService (LanguageService(..))
import Heco.Effectful.AccountService.Ldap (runLdapAccountServiceEx)
import Heco.Effectful.PrivilegeService (runSimplePrivilegeService)
import Heco.Effectful.LanguageService.OpenAI (OpenAIOps(..), runOpenAILanguageService)
import Heco.Effectful.LanguageService.Ollama (OllamaOps(..), runOllamaLanguageService)
import Heco.Effectful.LanguageToolProvider.Native (runNativeLanguageToolProviderEx)
import Heco.Effectful.DatabaseService.Milvus (runMilvusDatabaseServiceEx)
import Heco.Effectful.InternalTimeStream.RingBuffer (RingBufferOps(..), runRingBufferInternalTimeStreamEx)
import Heco.Effectful.Ego.Heco (runHecoEgoEx)
import Heco.Effectful.PortalService (runStandardPortalService, runPortal_)
import Heco.Agent.LanguageTools.Archives (archiveTools, ArchivesToolOps(..))
import Heco.Agent.Options (makeHecoOps, makeOpenAIOps, ollamaOps, ldapOps, milvusOps)
import Heco.Agent.AuthGroups (authGroups)

import Effectful (runEff, Eff, IOE, (:>))
import Effectful.Fail (runFailIO)
import Effectful.Concurrent (runConcurrent, threadDelay)
import Effectful.Dispatch.Dynamic (HasCallStack, reinterpret, send)
import Effectful.Error.Dynamic (CallStack, Error, runError)
import Effectful.Labeled (runLabeled, Labeled(Labeled))
import Effectful.Resource (runResource)
import Effectful.Reader.Dynamic (runReader)

import Data.Text (Text)
import Data.Aeson (FromJSON, ToJSON)
import Data.Default (Default(..))

import Control.Monad (forever)
import GHC.Generics (Generic)

runCombinedLanguageService ::
    (HasCallStack, IOE :> es)
    => OpenAIOps -> OllamaOps
    -> Eff (LanguageService : Event LanguageEvent : Error LanguageError : es) a
    -> Eff es (Either (CallStack, LanguageError) a)
runCombinedLanguageService openaiOps ollamaOps = 
    runError . runEvent . reinterpret evalInternal \_ -> \case
        Chat chatOps messages -> send $ Labeled @"openai" $ Chat chatOps messages
        Embed modelName text -> send $ Labeled @"ollama" $ Embed modelName text
        EmbedMany modelName texts -> send $ Labeled @"ollama" $ EmbedMany modelName texts
    where
        evalInternal =
            runLabeled @"openai" (runOpenAILanguageService openaiOps)
            . runLabeled @"ollama" (runOllamaLanguageService ollamaOps)

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

runHecoAgent :: IO ()
runHecoAgent = do
    hecoOps <- makeHecoOps
    openaiOps <- makeOpenAIOps
    let run = runEff . runFailIO . runConcurrent . runResource
            . runReader ArchivesToolOps
                { embeddingModel = "bge-m3"
                , collectionName = "archives" }
            . runSimplePrivilegeService authGroups
            . runThrowEither . runCombinedLanguageService openaiOps ollamaOps
            . runThrowEither . runLdapAccountServiceEx ldapOps
            . runThrowEither . runMilvusDatabaseServiceEx milvusOps
            . runThrowEither . runRingBufferInternalTimeStreamEx RingBufferOps { capacity = 20 }
            . runThrowEither . runNativeLanguageToolProviderEx
                archiveTools
            . runThrowEither . runHecoEgoEx hecoOps
            . runStandardPortalService
    _ <- run do
        -- embed "bge-m3" "介绍一下新艾利都" >>= liftIO . putStrLn . show
        -- content <- liftIO $ readFile "test.csv"
        -- let lineVec = V.fromList $ lines content
        --     contents = V.map (cast . StatementAction . T.pack) lineVec
        -- present_ contents
        -- testChat
        -- testHeco
        runPortal_ shellPortal
        runPortal_ $ makeOneBotPortal def
            { webapiUrl = "http://gilatod.local:3000"
            , websocketHost = "gilatod.local" }
        forever $ threadDelay maxBound
    pure ()