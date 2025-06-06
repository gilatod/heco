module Heco.Effectful.LanguageService.OpenAI
    ( -- * Data types
      OpenAIOps(..)
    , openaiOps
      -- * Language API
    , runOpenAILanguageService
    , runOpenAILanguageServiceEx
    ) where

import Heco.Network.HTTP.Client
    ( Headers, httpPost, httpRequestRaw )
import Heco.Data.FunctionSchema (FunctionSchema(..))
import Heco.Data.Aeson (defaultAesonOps)
import Heco.Data.Model (ModelName(ModelName))
import Heco.Data.Message (Message(..), ToolCall(..), ToolResponse(..), newAssistantMessage, messageUnique)
import Heco.Data.LanguageError (LanguageError(..))
import Heco.Data.Role (Role(..))
import Heco.Events.LanguageEvent (LanguageEvent(..))
import Heco.Effectful.HTTP (evalHttpManager)
import Heco.Effectful.Event (Event, trigger, runEvent)
import Heco.Effectful.LanguageService (LanguageService(..), ChatOps(..))
import Heco.Effectful.LanguageService.Common (unliftEventIO, relayError)

import Effectful (Eff, (:>), IOE, MonadIO (liftIO), runEff)
import Effectful.Dispatch.Dynamic (reinterpret)
import Effectful.Error.Dynamic (Error, HasCallStack, throwError, runError, CallStack)
import Effectful.Reader.Static (ask)
import Effectful.State.Static.Local (evalState, State, state, modify)

import Network.HTTP.Client
    ( withResponse,
      Response(responseBody),
      httpLbs,
      BodyReader,
      brConsume )
import Network.HTTP.Client.Conduit (bodyReaderSource)

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Unboxing qualified as VU
import Data.HashMap.Strict (HashMap)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Internal (c2w)

import Data.Aeson qualified as Aeson
import Data.Aeson.Text qualified as Aeson
import Data.Aeson (FromJSON(..), Object, Value (..))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.TH (deriveToJSON, deriveFromJSON, deriveJSON)

import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.Default (Default(..))
import Data.Cache.LRU (LRU)
import Data.Cache.LRU qualified as LRU
import Data.Unique (Unique)
import Data.Tuple (swap)

import Conduit (runConduit, (.|), await)
import Data.Conduit.Combinators (linesUnboundedAscii)

import Control.Exception (throw)
import Control.Monad.Extra (when, whenJust)
import GHC.Records (HasField)
import Pattern.Cast (cast)

data OpenAIOps = OpenAIOps
    { url :: Text
    , timeout :: Maybe Int
    , token :: Maybe Text
    , messageCacheLimit :: Int
    , maxEmbeddingLength :: Int }

openaiOps :: Text -> OpenAIOps
openaiOps url = OpenAIOps
    { url = url
    , timeout = Nothing
    , token = Nothing
    , messageCacheLimit = 64
    , maxEmbeddingLength = 4096 }

newtype OpenAIErrorMetadata = OpenAIErrorMetadata
    { raw :: Text }
    deriving (Show)

data OpenAIError = OpenAIError
    { message :: Text
    , metadata :: Maybe OpenAIErrorMetadata }
    deriving (Show)

deriveFromJSON defaultAesonOps ''OpenAIErrorMetadata
deriveFromJSON defaultAesonOps ''OpenAIError

data OpenAIFunctionToolCall = OpenAIFunctionToolCall
    { name :: Maybe Text
    , arguments :: Maybe Text }
    deriving (Show)

data OpenAIToolCall = OpenAIToolCall
    { id :: Text
    , index :: Int
    , function :: OpenAIFunctionToolCall }
    deriving (Show)

data OpenAIMessage = OpenAIMessage
    { role :: Maybe Role
    , content :: Maybe Text
    , reasoning :: Maybe Text
    , reasoning_content :: Maybe Text
    , tool_calls :: Maybe [OpenAIToolCall]
    , tool_call_id :: Maybe Text
    , name :: Maybe Text }
    deriving (Show)

instance Default OpenAIMessage where
    def = OpenAIMessage
        { role = Just System
        , reasoning = Nothing
        , reasoning_content = Nothing
        , content = Nothing
        , tool_calls = Nothing
        , tool_call_id = Nothing
        , name = Nothing }

newtype OpenAIChatChoice = OpenAIChatChoice
    { message :: OpenAIMessage }
    deriving (Show)

data OpenAIChatResp = OpenAIChatResp
    { choices :: Maybe [OpenAIChatChoice]
    , error :: Maybe OpenAIError }
    deriving (Show)

newtype OpenAIChatStreamChoice = OpenAIChatStreamChoice
    { delta :: OpenAIMessage }
    deriving (Show)

data OpenAIChatStreamResp = OpenAIChatStreamResp
    { choices :: Maybe [OpenAIChatStreamChoice]
    , error :: Maybe OpenAIError }
    deriving (Show)

deriveJSON defaultAesonOps ''OpenAIFunctionToolCall
deriveJSON defaultAesonOps ''OpenAIToolCall
deriveJSON defaultAesonOps ''OpenAIMessage
deriveFromJSON defaultAesonOps ''OpenAIChatChoice
deriveFromJSON defaultAesonOps ''OpenAIChatResp
deriveFromJSON defaultAesonOps ''OpenAIChatStreamChoice
deriveFromJSON defaultAesonOps ''OpenAIChatStreamResp

data OpenAIChatOps = OpenAIChatOps
    { model :: Text
    , messages :: Vector OpenAIMessage
    , tools :: Maybe [FunctionSchema]
    , stream :: Bool
    , temperature :: Maybe Float
    , top_p :: Maybe Float
    , max_token :: Maybe Int
    , presence_penalty :: Maybe Float
    , frequency_penalty :: Maybe Float }

deriveToJSON defaultAesonOps ''OpenAIChatOps

data OpenAIEmbeddingOps = OpenAIEmbeddingOps
    { model :: Text
    , input :: Vector Text
    , encoding_format :: Text }

newtype OpenAIEmbeddingData = OpenAIEmbeddingData
    { embedding :: VU.Vector Float }

data OpenAIEmbeddingResp = OpenAIEmbeddingResp
    { _data :: Maybe (Vector OpenAIEmbeddingData)
    , error :: Maybe OpenAIError }

deriveToJSON defaultAesonOps ''OpenAIEmbeddingOps
deriveFromJSON defaultAesonOps ''OpenAIEmbeddingData
deriveFromJSON defaultAesonOps ''OpenAIEmbeddingResp

type IsOpenAIResponse resp =
    ( FromJSON resp
    , HasField "error" resp (Maybe OpenAIError) )

checkResponse ::
    IsOpenAIResponse resp
    => resp -> Maybe LanguageError
checkResponse resp =
    case resp.error of
        Nothing -> Nothing
        Just err ->
            Just $ LanguageBackendError $ T.unpack $
                case err.metadata of
                    Nothing -> err.message
                    Just meta -> err.message <> ": " <> meta.raw

guardResponse ::
    ( HasCallStack
    , IsOpenAIResponse resp
    , Error LanguageError :> es )
    => resp -> Eff es ()
guardResponse resp = whenJust (checkResponse resp) throwError

guardResponseIO ::
    ( HasCallStack
    , IsOpenAIResponse resp )
    => resp -> IO ()
guardResponseIO resp = whenJust (checkResponse resp) throw

data ToolCallBuilder = ToolCallBuilder
    { id :: TLB.Builder
    , name :: TLB.Builder
    , arguments :: TLB.Builder }

instance Default ToolCallBuilder where
    def = ToolCallBuilder
        { id = mempty
        , name = mempty
        , arguments = mempty }

appendToolCall :: OpenAIToolCall -> ToolCallBuilder -> ToolCallBuilder
appendToolCall toolCall builder =
    let OpenAIToolCall { id = id, function = f } = toolCall
    in ToolCallBuilder
        { id = builder.id <> TLB.fromText id
        , name = maybe builder.name ((builder.name <>) . TLB.fromText) f.name
        , arguments = maybe builder.arguments ((builder.arguments <>) . TLB.fromText) f.arguments }

appendToolCalls :: [OpenAIToolCall] -> Vector ToolCallBuilder -> Vector ToolCallBuilder
appendToolCalls delta v =
    let currLen = V.length v
        maxIndex = delta & maximum . map \d -> d.index
        v' = if maxIndex < currLen
            then v
            else V.generate (maxIndex + 1) \i ->
                if i < currLen
                    then v V.! i
                    else def
    in V.accum (flip appendToolCall) v' $
        delta & map \d -> (d.index, d)

parseToolCallArguments :: TL.Text -> HashMap Text Value
parseToolCallArguments raw =
    maybe mempty KeyMap.toHashMapText
        (Aeson.decode @Object
            $ BS.toLazyByteString $ TL.encodeUtf8Builder raw)

fromToolCallBuilder :: ToolCallBuilder -> ToolCall
fromToolCallBuilder builder = ToolCall
    { id = TL.toStrict . TLB.toLazyText $ builder.id
    , name = TL.toStrict . TLB.toLazyText $ builder.name
    , arguments = parseToolCallArguments $ TLB.toLazyText builder.arguments }

fromCompleteOpenAIToolCall :: OpenAIToolCall -> ToolCall
fromCompleteOpenAIToolCall toolCall =
    let OpenAIToolCall { id = id, function = f } = toolCall
    in ToolCall
        { id = id
        , name = fromMaybe "" f.name
        , arguments = maybe mempty (parseToolCallArguments . TL.fromStrict) f.arguments }

data ChatStreamState = ChatStreamState
    { reasoning :: BS.Builder
    , statement :: BS.Builder
    , toolCalls :: Vector ToolCallBuilder }

instance Default ChatStreamState where
    def = ChatStreamState
        { reasoning = mempty
        , statement = mempty
        , toolCalls = mempty }

chatStreamStateToMessage :: IOE :> es => ChatStreamState -> Eff es Message
chatStreamStateToMessage state =
    newAssistantMessage statement reasoning toolCalls
    where
        statement = builderToText state.statement
        reasoning = builderToText state.reasoning
        toolCalls = V.toList $ V.map fromToolCallBuilder state.toolCalls
        builderToText = T.decodeUtf8 . BS.toStrict . BS.toLazyByteString

handleChatResponse ::
    ChatOps
    -> (LanguageEvent -> IO ())
    -> Response BodyReader
    -> IO Message
handleChatResponse chatOps triggerEvent response = do
    let body = responseBody response
    if chatOps.stream
        then do
            state <- runConduit $ bodyReaderSource body
                .| linesUnboundedAscii
                .| processLines def
            runEff $ chatStreamStateToMessage state
        else liftIO (brConsume body)
            >>= parseFull . BSL.fromChunks
    where
        splitData line =
            case BS.elemIndex (c2w ':') line of
                Nothing -> Nothing
                Just colonIndex ->
                    let head = BS.take colonIndex line
                        tail = BS.drop (colonIndex + 2) line
                    in Just (head, tail)

        processLines state = await >>= \case
            Nothing -> pure state
            Just line -> case splitData line of
                Just ("data", chunk) ->
                    if chunk == "[DONE]"
                        then pure state
                        else liftIO (parseChunk state $ BSL.fromStrict chunk)
                            >>= processLines
                _ -> case Aeson.eitherDecode @OpenAIChatStreamResp $ BSL.fromStrict line of
                    Left _ -> processLines state
                    Right res -> do
                        liftIO $ guardResponseIO res
                        pure state

        parseChunk state bs = Aeson.eitherDecode @OpenAIChatStreamResp bs & \case
            Left e -> throw $ LanguageBackendError e
            Right r -> do
                guardResponseIO r
                case r.choices of
                    Just (c:_) -> do
                        let delta = c.delta
                            reasoning = fromMaybe "" $ delta.reasoning <> delta.reasoning_content
                            statement = fromMaybe "" delta.content
                        when (T.length reasoning /= 0) $ onReasoningReceived reasoning
                        when (T.length statement /= 0) $ onStatementReceived statement
                        pure state
                            { reasoning = state.reasoning <> T.encodeUtf8Builder reasoning
                            , statement = state.statement <> T.encodeUtf8Builder statement
                            , toolCalls = maybe state.toolCalls (`appendToolCalls` state.toolCalls) delta.tool_calls }
                    _ -> throw $ LanguageBackendError "no message received"

        parseFull bs = Aeson.eitherDecode @OpenAIChatResp bs & \case
            Left e -> throw $ LanguageBackendError e
            Right r -> do
                guardResponseIO r
                case r.choices of
                    Just (c:_) -> do
                        let message = c.message
                            statement = fromMaybe "" message.content
                            reasoning = fromMaybe "" message.reasoning
                        when (T.length reasoning /= 0) $ onReasoningReceived reasoning
                        when (T.length statement /= 0) $ onStatementReceived statement
                        runEff $ newAssistantMessage statement reasoning $
                            maybe [] (map fromCompleteOpenAIToolCall) message.tool_calls
                    _ -> throw $ LanguageBackendError "no message received"

        onReasoningReceived = triggerEvent . OnReasoningChunkReceived
        onStatementReceived = triggerEvent . OnStatementChunkReceived

encodeMessage ::
    ( HasCallStack
    , State (LRU Unique OpenAIMessage) :> es )
    => Message -> Eff es OpenAIMessage
encodeMessage msg = do
    let msgUnique = messageUnique msg
    cache <- state $ swap . LRU.lookup msgUnique
    case cache of
        Nothing -> do
            let res = doEncode msg
            modify $ LRU.insert msgUnique res
            pure res
        Just content -> pure content
    where
        doEncode = \case
            SystemMessage _ t -> def
                { role = Just System
                , content = Just t }
            UserMessage _ t -> def
                { role = Just User
                , content = Just t }
            ToolMessage _ r ->
                let json = TL.toStrict $ Aeson.encodeToLazyText r.content
                in def
                    { role = Just Tool
                    , tool_call_id = Just r.id
                    , name = Just r.name
                    , content = Just json }
            AssistantMessage _ statement reasoning toolCalls -> def
                { role = Just Assistant
                , content = Just statement
                , reasoning = Just reasoning
                , tool_calls = case toolCalls of
                    [] -> Nothing
                    _ -> Just $ zipWith (curry encodeToolCall) [0..] toolCalls }

        encodeToolCall (i, t) = OpenAIToolCall
            { id = t.id
            , index = i
            , function = OpenAIFunctionToolCall
                { name = Just t.name
                , arguments = Just $ TL.toStrict $ Aeson.encodeToLazyText t.arguments } }

runOpenAILanguageService ::
    ( HasCallStack
    , IOE :> es
    , Event LanguageEvent :> es
    , Error LanguageError :> es )
    => OpenAIOps -> Eff (LanguageService : es) a -> Eff es a
runOpenAILanguageService ops = reinterpret wrap \env -> \case
    Chat chatOps messages -> do
        manager <- ask
        encodedMsgs <- V.mapM encodeMessage messages

        let req = OpenAIChatOps
                { model = cast chatOps.modelName
                , messages = encodedMsgs
                , tools = case chatOps.tools of
                    [] -> Nothing
                    ts -> Just ts
                , stream = chatOps.stream
                , temperature = chatOps.temperature
                , top_p = chatOps.topP
                , max_token = chatOps.maxToken
                , presence_penalty = chatOps.presencePenalty
                , frequency_penalty = chatOps.frequencyPenalty }
            reqJSON =
                let bs = Aeson.encode req
                    extra = chatOps.extra
                in if KeyMap.null extra
                    then bs
                    else BSL.take (BSL.length bs - 1) bs <> "," <> BSL.drop 1 (Aeson.encode extra)

        msg <- unliftEventIO env \unlift -> do
            req' <- httpRequestRaw "POST" (ops.url <> "/chat/completions") headers reqJSON
            withResponse req' manager $
                handleChatResponse chatOps (unlift . trigger)
        trigger $ OnMessageReceived msg
        pure msg

    Embed (ModelName name) text -> do
        let vec = V.singleton $ T.take ops.maxEmbeddingLength text
        embeddings <- embedImpl ops OpenAIEmbeddingOps
            { model = name
            , input = vec
            , encoding_format = "float" }
        trigger $ OnEmbeddingsReceived vec embeddings
        pure $ embeddings V.! 0

    EmbedMany (ModelName name) texts -> do
        embeddings <- embedImpl ops OpenAIEmbeddingOps
            { model = name
            , input = V.map (T.take ops.maxEmbeddingLength) texts
            , encoding_format = "float" }
        trigger $ OnEmbeddingsReceived texts embeddings
        pure embeddings

    where
        wrap =
            let msgCacheLimit = Just $ toInteger ops.messageCacheLimit
            in evalHttpManager ops.timeout
                . evalState (LRU.newLRU @Unique @OpenAIMessage msgCacheLimit)

        embedImpl ops req = do
            manager <- ask
            embeddingsRaw <-
                liftIO (relayError do
                    req' <- httpPost (ops.url <> "/embeddings") headers req
                    responseBody <$> httpLbs req' manager)
                >>= either throwError pure

            case Aeson.eitherDecode @OpenAIEmbeddingResp embeddingsRaw of
                Left e -> throwError $ LanguageBackendError e
                Right r -> do
                    guardResponse r
                    case r._data of
                        Nothing -> throwError $ LanguageBackendError "embedding not received"
                        Just ds -> do
                            let embeddings = V.map (\d -> d.embedding) ds
                            pure embeddings

        headers :: Headers
        headers = ("Content-Type", "application/json")
            : ("Accept", "application/json")
            : maybe [] (\t -> [("Authorization", "Bearer " <> T.encodeUtf8 t)]) ops.token

runOpenAILanguageServiceEx ::
    (HasCallStack, IOE :> es)
    => OpenAIOps
    -> Eff (LanguageService : Event LanguageEvent : Error LanguageError : es) a
    -> Eff es (Either (CallStack, LanguageError) a)
runOpenAILanguageServiceEx ops =
    runError . runEvent . runOpenAILanguageService ops