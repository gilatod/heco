{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# LANGUAGE DeriveAnyClass #-}

module Main (main) where

import Heco.Data.FunctionSchema
    ( EnumDefaultDesc, FieldDesc, HasDataSchema, RecordDefault )
import Heco.Data.Default ()
import Heco.Data.Aeson (HasAesonOps, AesonDefault(..))
import Heco.Data.AuthGroup (AuthGroup(..))
import Heco.Data.Message (Message(..), newSystemMessage, newUserMessage)
import Heco.Data.Embedding (Embedding(Embedding))
import Heco.Data.Entity (EntityId)
import Heco.Data.Entity.TH (deriveEntity)
import Heco.Data.Immanant.Terminal (Terminal(..))
import Heco.Data.LanguageTool (LanguageTool(..), LanguageToolSpec(..), Param, ParamDesc, Ret)
import Heco.Data.LanguageError (LanguageError(..))
import Heco.Data.LanguageToolError (LanguageToolError(..))
import Heco.Data.Portal.Shell (shellPortal)
import Heco.Events.LanguageEvent (LanguageEvent(..))
import Heco.Effectful.Exception (runThrowEither)
import Heco.Effectful.Event (on, Event, runEvent)
import Heco.Effectful.LanguageService (embed, chat, LanguageService(..), ChatOps(..))
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
    ( DatabaseService,
      addEntity,
      getEntities,
      loadCollection,
      setEntity_,
      SearchOps(rangeFilter, limit, radius) )
import Heco.Effectful.InternalTimeStream (InternalTimeStream, present_)
import Heco.Effectful.InternalTimeStream.RingBuffer (RingBufferOps (RingBufferOps, capacity), runRingBufferInternalTimeStreamEx)
import Heco.Effectful.Ego (Ego, interactEgo)
import Heco.Effectful.Ego.Heco (runHecoEgoEx, HecoOps(..), immanantContentXMLFormatter, hecoMemoryOps, HecoMemoryOps(..))
import Heco.Effectful.PortalService (runStandardPortalService, runPortal)

import Effectful (runEff, liftIO, Eff, IOE, (:>))
import Effectful.Fail (runFailIO)
import Effectful.Concurrent (runConcurrent, threadDelay)
import Effectful.Dispatch.Dynamic (HasCallStack, reinterpret, send)
import Effectful.State.Static.Local (evalState, get, put)
import Effectful.Error.Dynamic (CallStack, Error, runError)
import Effectful.Labeled (runLabeled, Labeled (Labeled))

import Data.Default (Default(..))
import Data.HashSet qualified as HashSet
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Vector qualified as V
import Data.Vector.Unboxing qualified as VU
import Data.Aeson (FromJSON, ToJSON)

import Control.Monad (when, forM_, forever)
import System.IO (stdout)
import GHC.IO.Handle (hFlush)
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

testChat ::
    ( HasCallStack
    , LanguageService :> es
    , Event LanguageEvent :> es
    , AccountService :> es
    , IOE :> es )
    => Eff es ()
testChat = do
    token <- login $ UsernameLoginOps "test" "Holders-instance-14-sulfur"
    prompt <- newSystemMessage "You are a helpful assistant."
    evalState True $ doChat token (V.singleton prompt)
        `on` \case
            OnReasoningChunkReceived content -> liftIO do
                T.putStr content
                hFlush stdout
            OnUtteranceChunkReceived content -> do
                reasoning <- get
                when reasoning do
                    put False
                    liftIO $ putStrLn "==============="
                liftIO $ T.putStr content
                liftIO $ hFlush stdout
            OnMessageReceived (AssistantMessage _ _ toolCalls) -> do
                forM_ toolCalls \toolCall ->
                    liftIO $ putStrLn $ show toolCall
                liftIO $ putStrLn ""
            _ -> pure ()
    where
        doChat token messages = do
            liftIO $ putStr "> " >> hFlush stdout
            input <- liftIO $ getLine
            put True
            msg <- newUserMessage $ T.pack input
            let messages' = V.snoc messages msg
            msg <- chat ops messages'
            doChat token $ V.snoc messages' msg

        ops = (chatOps "deepseek/deepseek-chat-v3-0324:free")
            { stream = True }

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
    , IOE :> es )
    => Eff es ()
testMilvus = do
    Embedding vector <- embed "mxbai-embed-large" "你好世界"
    liftIO . putStrLn . show $ VU.length vector

    loadCollection "memory"

    eid <- addEntity "memory" TestEntity
        { id = Nothing
        , vector = Just vector
        , text = "hello world" }

    eid2 <- addEntity "memory" TestEntity
        { id = Nothing
        , vector = Just vector
        , text = "hello world xxxx" }

    setEntity_ "memory" TestEntity
        { id = Just eid
        , vector = Just vector
        , text = "hello world 2" }

    res <- getEntities @TestEntity "memory" $ VU.fromList [eid, eid2]
    liftIO $ putStrLn $ show res
    pure()

testHeco ::
    ( HasCallStack
    , IOE :> es
    , InternalTimeStream :> es
    , Ego :> es
    , Event LanguageEvent :> es )
    => Eff es ()
testHeco = doChat
    `on` \case
        OnUtteranceChunkReceived content -> do
            liftIO $ T.putStr content
            liftIO $ hFlush stdout
        OnMessageReceived (AssistantMessage _ _ toolCalls) -> do
            forM_ toolCalls \toolCall ->
                liftIO $ putStrLn $ show toolCall
            liftIO $ putStrLn ""
        _ -> pure ()
    where
        doChat = do
            liftIO $ putStr "> " >> hFlush stdout
            input <- liftIO $ getLine
            interactEgo do
                present_ $ V.fromList
                    [ cast $ TerminalChat 1 $ "User: " <> T.pack input ]
            doChat

newHecoOps :: IO HecoOps
newHecoOps = do
    characterPrompt <- readFile "./prompts/character.md"
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
    let run = runEff . runFailIO . runConcurrent
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
        forever $ threadDelay maxBound
    pure ()