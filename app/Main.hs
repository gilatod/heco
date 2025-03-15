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
import Heco.Data.Collection (CollectionName(CollectionName))
import Heco.Data.TimePhase (ImmanantContent(..), SenseData(..))
import Heco.Data.Model (ModelName(ModelName))
import Heco.Data.LanguageError (LanguageError)
import Heco.Events.LanguageEvent (LanguageEvent(..))
import Heco.Events.EgoEvent (EgoEvent(..))
import Heco.Effectful.Exception (eitherThrowIO)
import Heco.Effectful.Event (on, Event, runEvent)
import Heco.Effectful.LanguageService (embed, chat, LanguageService(..))
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
import Heco.Effectful.LanguageService (chatOps, ChatOps(..))
import Heco.Effectful.LanguageService.OpenAI (runOpenAILanguageServiceEx, OpenAIOps(..), OpenAIMessage (reasoning), runOpenAILanguageService)
import Heco.Effectful.LanguageService.Ollama (runOllamaLanguageServiceEx, OllamaOps(..), runOllamaLanguageService)
import Heco.Effectful.DatabaseService.Milvus (runMilvusDatabaseServiceEx, MilvusOps(..))
import Heco.Effectful.DatabaseService
    ( DatabaseService,
      addEntity,
      getEntities,
      loadCollection, setEntity_, addEntity_ )
import Heco.Effectful.InternalTimeStream.RingBuffer (RingBufferOps (RingBufferOps, capacity), runRingBufferInternalTimeStreamEx)
import Heco.Effectful.Ego (Ego, interactEgo)
import Heco.Effectful.Ego.Heco (runHecoEgoEx, HecoOps(..), MemoryEntity(..))
import Heco.Effectful.InternalTimeStream (InternalTimeStream, enrichUrimpression_)

import Effectful (runEff, liftIO, Eff, IOE, (:>))
import Effectful.Fail (Fail, runFailIO)
import Effectful.Concurrent (runConcurrent)
import Effectful.Dispatch.Dynamic (HasCallStack, reinterpret, send)
import Effectful.State.Static.Local (evalState, get, put)
import Effectful.Error.Dynamic (CallStack, Error, runError)
import Effectful.Labeled (runLabeled, Labeled (Labeled))

import Data.Default (Default(..))
import Data.HashSet qualified as HashSet
import Data.List.NonEmpty (appendList, singleton)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Vector qualified as V
import Data.Vector.Unboxing qualified as VU

import GHC.IO.Handle (hFlush)
import GHC.Generics (Generic)
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

createOpenAIOps :: IO OpenAIOps
createOpenAIOps = do
    token <- readFile "./tokens/openrouter.txt"
    pure OpenAIOps
        { url = "https://openrouter.ai/api"
        , timeout = Nothing
        , token = Just $ T.pack token }

milvusOps :: MilvusOps
milvusOps = MilvusOps
    { url = "http://localhost:19530"
    , timeout = Nothing
    , token = Nothing
    , database = Just "heco" }

createHecoOps :: IO HecoOps
createHecoOps = do
    characterPrompt <- readFile "./prompts/character.md"
    interactionMainPrompt <- readFile "./prompts/interaction_main.md"
    retentionPrompt <- readFile "./prompts/retention.md"
    urimpressionPrompt <- readFile "./prompts/urimpression.md"
    pure HecoOps
        { characterPrompt = T.pack characterPrompt
        , interactionMainPrompt = T.pack interactionMainPrompt
        , retentionPrompt = T.pack retentionPrompt
        , urimpressionPrompt = T.pack urimpressionPrompt
        , toolsPrompt = ""
        , memoryCollection = CollectionName "main"
        , memorySearchLimit = Just 10
        , chatOps = (chatOps "deepseek/deepseek-r1-distill-qwen-32b:free")
            { temperature = Just 1
            , topP = Just 0.95 }
        , memoryEmbeddingModel = ModelName "mxbai-embed-large" }

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
    prompt <- Message System <$> (liftIO . fmap T.pack . readFile $ "prompt.txt")
    evalState True $ doChat token (singleton prompt)
        `on` \case
            OnReasoningChunkReceived msg -> liftIO do
                T.putStr $ msg.content
                hFlush stdout
            OnDiscourseChunkReceived msg -> do
                reasoning <- get
                when reasoning do
                    put False
                    liftIO $ putStrLn "==============="
                liftIO $ T.putStr $ msg.content
                liftIO $ hFlush stdout
            OnDiscourseResponseReceived _ -> liftIO $ putStrLn ""
            _ -> pure ()
    where
        doChat token messages = do
            liftIO $ putStr "> " >> hFlush stdout
            input <- liftIO $ getLine
            put True
            let messages' = appendList messages [Message User $ T.pack input]
            msg <- chat r1ChatOps messages'
            doChat token $ appendList messages' [msg]
        r1ChatOps = chatOps "deepseek/deepseek-r1-distill-llama-70b:free"

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

testHeco ::
    ( HasCallStack
    , IOE :> es
    , InternalTimeStream :> es
    , Ego :> es
    , Event LanguageEvent :> es
    , Event EgoEvent :> es )
    => Eff es ()
testHeco = evalState True $ doChat
        `on` \case
            OnEgoTaskGenerated msg -> liftIO do
                T.putStrLn msg
            _ -> pure ()
        `on` \case
            OnReasoningChunkReceived msg -> liftIO do
                T.putStr $ msg.content
                hFlush stdout
            OnDiscourseChunkReceived msg -> do
                reasoning <- get
                when reasoning do
                    put False
                    liftIO $ putStrLn "==============="
                liftIO $ T.putStr $ msg.content
                liftIO $ hFlush stdout
            OnDiscourseResponseReceived _ -> liftIO $ putStrLn ""
            _ -> pure ()
    where
        doChat = do
            liftIO $ putStr "> " >> hFlush stdout
            input <- liftIO $ getLine
            put True
            interactEgo do
                enrichUrimpression_ $ V.singleton
                    $ SenseDataContent $ OlfactorySenseData "泥土的气味"
                enrichUrimpression_ $ V.singleton
                    $ SenseDataContent $ AcousticSenseData "细微的雨声"
                enrichUrimpression_ $ V.singleton
                    $ SenseDataContent $ VisualSenseData $ "法厄同：" <> T.pack input
            doChat
main :: IO ()
main = do
    hecoOps <- createHecoOps
    openaiOps <- createOpenAIOps
    let run = runEff . runFailIO . runConcurrent
            . runSimplePrivilegeService groups
            . eitherThrowIO . runCombinedLanguageService openaiOps ollamaOps
            . eitherThrowIO . runLdapAccountServiceEx ldapOps
            . eitherThrowIO . runMilvusDatabaseServiceEx milvusOps
            . eitherThrowIO . runRingBufferInternalTimeStreamEx RingBufferOps { capacity = 30 }
            . eitherThrowIO . runHecoEgoEx hecoOps
    _ <- run do
        -- [Embedding vector] <- embed "mxbai-embed-large" ["雨声"]
        -- addEntity_ "main" MemoryEntity
        --     { id = Nothing
        --     , vector = Just vector
        --     , create_time = Nothing
        --     , _type = "accustic_data"
        --     , content = "Fairy 讨厌雨声" }
        testHeco
    pure ()