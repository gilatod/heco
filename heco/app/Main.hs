{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# LANGUAGE DeriveAnyClass #-}

module Main (main) where

import Heco.Data.FunctionSchema
    ( EnumDefaultDesc, FieldDesc, HasDataSchema, RecordDefault )
import Heco.Data.Default ()
import Heco.Data.Aeson (HasAesonOps, AesonDefault(..))
import Heco.Data.AuthGroup (AuthGroup(..))
import Heco.Data.LanguageTool (LanguageTool(..), LanguageToolSpec(..), Param, ParamDesc, Ret)
import Heco.Data.LanguageError (LanguageError(..))
import Heco.Data.LanguageToolError (LanguageToolError(..))
import Heco.Data.Portal.Shell (shellPortal)
import Heco.Data.Portal.OneBot (makeOneBotPortal, OneBotOps(..))
import Heco.Events.LanguageEvent (LanguageEvent(..))
import Heco.Effectful.Exception (runThrowEither)
import Heco.Effectful.Event (Event, runEvent)
import Heco.Effectful.LanguageService (LanguageService(..))
import Heco.Effectful.AccountService (LoginOps(..), login, getUser, logout, AccountService)
import Heco.Effectful.AccountService.Ldap
    ( runLdapAccountServiceEx,
      LdapGroupAttributes(member, LdapGroupAttributes, name),
      LdapGroupMemberIdentification(GroupMemberIdentifiedByDn),
      LdapOps(..),
      LdapUserAttributes(email, LdapUserAttributes, username, nickname),
      Password(Password),
      Dn(Dn),
      Host(Tls) )
import Heco.Effectful.PrivilegeService (runSimplePrivilegeService)
import Heco.Effectful.SessionContext (getSessionContext, SessionContext)
import Heco.Effectful.LanguageService (chatOps)
import Heco.Effectful.LanguageService.OpenAI (OpenAIOps(..), runOpenAILanguageService, openaiOps)
import Heco.Effectful.LanguageService.Ollama (OllamaOps(..), runOllamaLanguageService)
import Heco.Effectful.LanguageToolProvider.Native (runNativeLanguageToolProviderEx)
import Heco.Effectful.DatabaseService.Milvus (runMilvusDatabaseServiceEx, MilvusOps(..))
import Heco.Effectful.DatabaseService
    ( SearchOps(rangeFilter, limit, radius) )
import Heco.Effectful.InternalTimeStream.RingBuffer (RingBufferOps (RingBufferOps, capacity), runRingBufferInternalTimeStreamEx)
import Heco.Effectful.Ego.Heco (runHecoEgoEx, HecoOps(..), immanantContentXMLFormatter, hecoMemoryOps, HecoMemoryOps(..))
import Heco.Effectful.PortalService (runStandardPortalService, runPortal)

import Effectful (runEff, liftIO, Eff, IOE, (:>))
import Effectful.Fail (runFailIO)
import Effectful.Concurrent (runConcurrent, threadDelay)
import Effectful.Dispatch.Dynamic (HasCallStack, reinterpret, send)
import Effectful.Error.Dynamic (CallStack, Error, runError)
import Effectful.Labeled (runLabeled, Labeled (Labeled))
import Effectful.Resource (runResource)

import Data.Default (Default(..))
import Data.HashSet qualified as HashSet
import Data.Text (Text)
import Data.Text qualified as T
import Data.Aeson (FromJSON, ToJSON)

import Control.Monad (forever)
import GHC.Generics (Generic)
import Pattern.Cast (Cast(cast))

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

newOpenAIOps :: IO OpenAIOps
newOpenAIOps = do
    token <- readFile "./tokens/openrouter.txt"
    pure $ (openaiOps "https://openrouter.ai/api/v1")
        { token = Just $ T.pack token }

milvusOps :: MilvusOps
milvusOps = MilvusOps
    { url = "http://localhost:19530/v2"
    , timeout = Nothing
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
    ctx <- getSessionContext @Int token
    liftIO $ putStrLn $ show ctx
    logout token
    pure ()

newHecoOps :: IO HecoOps
newHecoOps = do
    characterPrompt <- readFile "./prompts/characters/heco.md"
    taskPrompt <- readFile "./prompts/task.md"
    pure HecoOps
        { characterPrompt = T.pack characterPrompt
        , taskPrompt = T.pack taskPrompt
        , chatOps = (chatOps "deepseek/deepseek-chat-v3-0324")
        , immanantContentFormatter = immanantContentXMLFormatter
        , memoryOps = (hecoMemoryOps "memory" "bge-m3")
            { searchOps = def
                { limit = Just 10
                , radius = Just 0.05
                , rangeFilter = Just 1 } }
        , messageCacheLimit = Just 64 }

data Location = Location
    { name :: FieldDesc (Maybe Text) "Name of location, e.g. San Francisco, CA"
    , datetime :: FieldDesc (Maybe Text) "Date or time, e.g. today, tomorrow, 2023-06-29" }
    -- HasAesonOps 为 Location 提供 JSON 配置
    deriving (Generic, HasAesonOps)
    -- 用 HasAesonOps 里指定的配置来实现 JSON 解码 / 编码
    deriving (FromJSON, ToJSON) via (AesonDefault Location)
    -- 用 HasAesonOps 里指定的配置来实现 JSON Schema 自动创建
    deriving HasDataSchema via (RecordDefault Location)

data TestEnum
    = A | B | C
    deriving (Generic, Show, Enum, Bounded)
    deriving (HasAesonOps, FromJSON, ToJSON) via (AesonDefault TestEnum)
    deriving HasDataSchema via
        (EnumDefaultDesc TestEnum
            [ "This is A"
            , "This is B"
            , "This is C" ])

weatherTool :: LanguageTool es
    "get_weather"
    "Get weather from given locations and datetimes"
    (Param "locations" [Location] -> Ret [Text])
weatherTool = LanguageTool \locations -> pure ["sunny"]

adderTool :: LanguageTool es
    "adder"
    "Add two numbers together"
    (ParamDesc "a" Float "Number 1" -> ParamDesc "b" Float "Number 2" -> Ret Float)
adderTool = LanguageTool \a b -> pure $ a + b

languageTools :: forall es. Error LanguageToolError :> es => [LanguageToolSpec es]
languageTools =
    [ cast $ weatherTool @es
    , cast $ adderTool @es ]

main :: IO ()
main = do
    hecoOps <- newHecoOps
    openaiOps <- newOpenAIOps
    let run = runEff . runFailIO . runConcurrent . runResource
            . runSimplePrivilegeService groups
            . runThrowEither . runCombinedLanguageService openaiOps ollamaOps
            . runThrowEither . runLdapAccountServiceEx ldapOps
            . runThrowEither . runMilvusDatabaseServiceEx milvusOps
            . runThrowEither . runRingBufferInternalTimeStreamEx RingBufferOps { capacity = 20 }
            . runThrowEither . runNativeLanguageToolProviderEx languageTools
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
        _ <- runPortal shellPortal
        _ <- runPortal $ makeOneBotPortal def
            { webapiUrl = "http://gilatod.local:3000"
            , websocketHost = "gilatod.local" }
        forever $ threadDelay maxBound
    pure ()