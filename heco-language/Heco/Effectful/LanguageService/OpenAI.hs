module Heco.Effectful.LanguageService.OpenAI where

import Heco.Network.HTTP.Client (Headers)
import Heco.Data.Aeson (defaultAesonOps)
import Heco.Data.Model (ModelName(ModelName))
import Heco.Data.Role (Role(..))
import Heco.Data.Message (Message(..))
import Heco.Data.LanguageError (LanguageError(..))
import Heco.Data.Embedding (Embedding(Embedding))
import Heco.Events.LanguageEvent (LanguageEvent(..))
import Heco.Effectful.HTTP (evalHttpManager)
import Heco.Effectful.Event (Event, trigger, runEvent)
import Heco.Effectful.LanguageService (LanguageService(..), ChatOps(..))
import Heco.Effectful.LanguageService.Common (unliftEventIO, relayError)
import Heco.Network.HTTP.Client (httpPost)

import Effectful (Eff, (:>), IOE, MonadIO (liftIO))
import Effectful.Dispatch.Dynamic (reinterpret)
import Effectful.Error.Dynamic (Error, HasCallStack, throwError, runError, CallStack)
import Effectful.Reader.Static (ask, Reader)

import Network.HTTP.Client
    ( withResponse,
      Response(responseBody),
      httpLbs,
      Manager,
      BodyReader,
      brConsume )
import Network.HTTP.Client.Conduit (bodyReaderSource)

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Unboxing qualified as VU
import Data.List.NonEmpty (NonEmpty)
import Data.Coerce (coerce)

import Data.ByteString qualified as BS
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Internal (c2w)

import Data.Aeson qualified as Aeson
import Data.Aeson (FromJSON(..))
import Data.Aeson.TH (deriveToJSON, deriveFromJSON)

import Control.Exception (throw)
import Control.Monad.Extra (when)
import GHC.Generics (Generic)
import GHC.Records (HasField)
import Pattern.Cast (cast)
import Conduit (runConduit, (.|), await)
import Data.Conduit.Combinators (linesUnboundedAscii)

data OpenAIOps = OpenAIOps
    { url :: Text
    , timeout :: Maybe Int
    , token :: Maybe Text }

openaiOps :: Text -> OpenAIOps
openaiOps url = OpenAIOps
    { url = url
    , timeout = Nothing
    , token = Nothing }

data OpenAIError = OpenAIError
    { message :: String }

deriveFromJSON defaultAesonOps ''OpenAIError

data OpenAIChatOps = OpenAIChatOps
    { model :: Text
    , messages :: NonEmpty Message
    , stream :: Bool
    , temperature :: Maybe Float
    , top_p :: Maybe Float
    , max_token :: Maybe Int
    , presence_penalty :: Maybe Float
    , frequency_penalty :: Maybe Float }

data OpenAIMessage = OpenAIMessage
    { role :: Role
    , content :: Text
    , reasoning :: Maybe Text }
    deriving (Eq, Show, Generic)

data OpenAIChatChoice = OpenAIChatChoice
    { message :: OpenAIMessage }

data OpenAIChatResp = OpenAIChatResp
    { choices :: Maybe [OpenAIChatChoice]
    , error :: Maybe OpenAIError }

data OpenAIChatStreamChoice = OpenAIChatStreamChoice
    { delta :: OpenAIMessage }

data OpenAIChatStreamResp = OpenAIChatStreamResp
    { choices :: Maybe [OpenAIChatStreamChoice]
    , error :: Maybe OpenAIError }

deriveToJSON defaultAesonOps ''OpenAIChatOps
deriveFromJSON defaultAesonOps ''OpenAIMessage
deriveFromJSON defaultAesonOps ''OpenAIChatChoice
deriveFromJSON defaultAesonOps ''OpenAIChatResp
deriveFromJSON defaultAesonOps ''OpenAIChatStreamChoice
deriveFromJSON defaultAesonOps ''OpenAIChatStreamResp

data OpenAIEmbeddingOps = OpenAIEmbeddingOps
    { model :: Text
    , input :: Vector Text
    , encoding_format :: Text }

data OepnAIEmbeddingData = OpenAIEmbeddingData
    { embedding :: VU.Vector Float }

data OpenAIEmbeddingResp = OpenAIEmbeddingResp
    { _data :: Maybe (Vector OepnAIEmbeddingData)
    , error :: Maybe OpenAIError }

deriveToJSON defaultAesonOps ''OpenAIEmbeddingOps
deriveFromJSON defaultAesonOps ''OepnAIEmbeddingData
deriveFromJSON defaultAesonOps ''OpenAIEmbeddingResp

type IsOpenAIResponse resp =
    ( FromJSON resp
    , HasField "error" resp (Maybe OpenAIError) )

guardResponse ::
    ( HasCallStack
    , IsOpenAIResponse resp
    , Error LanguageError :> es )
    => resp -> Eff es ()
guardResponse resp =
    case resp.error of
        Nothing -> pure ()
        Just err -> throwError $ LanguageBackendError err.message

guardResponseIO ::
    ( HasCallStack
    , IsOpenAIResponse resp )
    => resp -> IO ()
guardResponseIO resp =
    case resp.error of
        Nothing -> pure ()
        Just err -> throw $ LanguageBackendError err.message

handleChatResponse ::
    ChatOps
    -> (LanguageEvent -> IO ())
    -> Response BodyReader
    -> IO (Text, Text)
handleChatResponse chatOps triggerEvent response = do
    let body = responseBody response
    if chatOps.stream
        then do
            (rb, db) <- runConduit $ bodyReaderSource body
                .| linesUnboundedAscii
                .| processLines mempty
            pure (builderToText rb, builderToText db)
        else brConsume body
            >>= parseFull . BSL.fromChunks
    where
        builderToText = T.decodeUtf8 . BS.toStrict . toLazyByteString
 
        splitData line =
            case BS.elemIndex (c2w ':') line of
                Nothing -> Nothing
                Just colonIndex ->
                    let head = BS.take colonIndex line
                        tail = BS.drop (colonIndex + 2) line
                    in Just (head, tail)

        processLines builders = await >>= \case
            Nothing -> pure builders
            Just line -> case splitData line of
                Just ("data", chunk) ->
                    if chunk == "[DONE]"
                        then pure builders
                        else (liftIO $ parseChunk builders $ BSL.fromStrict chunk)
                            >>= processLines
                _ -> processLines builders

        parseChunk (rb, db) bs = either
            (\e -> throw $ LanguageBackendError e)
            (\r -> do
                guardResponseIO r
                case r.choices of
                    Just (c:_) -> do
                        let delta = c.delta
                            reasoning = maybe "" id delta.reasoning
                            content = delta.content
                        when (T.length reasoning /= 0) $ onReasoningReceived reasoning
                        when (T.length content /= 0) $ onDiscourseReceived content
                        pure (rb <> T.encodeUtf8Builder reasoning
                            , db <> T.encodeUtf8Builder content)
                    _ -> throw $ LanguageBackendError "no message received")
            $ Aeson.eitherDecode @OpenAIChatStreamResp bs

        parseFull bs = either
            (\e -> throw $ LanguageBackendError e)
            (\r -> do
                guardResponseIO r
                case r.choices of
                    Just (c:_) -> do
                        let message = c.message
                            reasoning = maybe "" id message.reasoning
                            content = message.content
                        when (T.length reasoning /= 0) $ onReasoningReceived reasoning
                        when (T.length content /= 0) $ onDiscourseReceived content
                        pure (reasoning, content)
                    _ -> throw $ LanguageBackendError "no message received")
            $ Aeson.eitherDecode @OpenAIChatResp bs

        onReasoningReceived = triggerEvent . OnReasoningChunkReceived . Message Assistant
        onDiscourseReceived = triggerEvent . OnDiscourseChunkReceived . Message Assistant

embedImpl ::
    ( HasCallStack
    , IOE :> es
    , Error LanguageError :> es
    , Reader Manager :> es )
    => OpenAIOps -> OpenAIEmbeddingOps -> Eff es (Vector Embedding)
embedImpl ops req = do
    manager <- ask
    embeddingsRaw <-
        (liftIO $ relayError do
            req' <- httpPost (ops.url <> "/api/embed") [] req
            responseBody <$> httpLbs req' manager)
        >>= either throwError pure
    
    case Aeson.eitherDecode @OpenAIEmbeddingResp embeddingsRaw of
        Left e -> throwError $ LanguageBackendError e
        Right r -> do
            guardResponse r
            case r._data of
                Nothing -> throwError $ LanguageBackendError "embedding not received"
                Just ds ->
                    let embeddings = V.map (\d -> d.embedding) ds
                    in pure (coerce embeddings :: Vector Embedding)

runOpenAILanguageService ::
    ( HasCallStack
    , IOE :> es
    , Event LanguageEvent :> es
    , Error LanguageError :> es )
    => OpenAIOps -> Eff (LanguageService : es) a -> Eff es a
runOpenAILanguageService ops = reinterpret (evalHttpManager ops.timeout) \env -> \case
    Chat chatOps messages -> do
        manager <- ask

        let req = OpenAIChatOps
                { model = cast chatOps.modelName
                , messages = messages
                , stream = chatOps.stream
                , temperature = chatOps.temperature
                , top_p = chatOps.topP
                , max_token = chatOps.maxToken
                , presence_penalty = chatOps.presencePenalty
                , frequency_penalty = chatOps.frequencyPenalty }

        (reasoning, discourse) <- unliftEventIO env \unlift -> do
            req' <- httpPost (ops.url <> "/v1/chat/completions") headers req
            withResponse req' manager $ handleChatResponse chatOps (unlift . trigger)
        
        when (T.length reasoning /= 0)
            $ trigger $ OnReasoningResponseReceived $ Message Assistant reasoning

        let msg = Message Assistant discourse
        trigger $ OnDiscourseResponseReceived msg
        pure msg

    Embed (ModelName name) text -> do
        let vec = V.singleton text
        embeddings <- embedImpl ops OpenAIEmbeddingOps
            { model = name
            , input = vec
            , encoding_format = "float" }
        trigger $ OnEmbeddingsReceived vec embeddings
        pure $ embeddings V.! 0

    EmbedMany (ModelName name) texts -> do
        embeddings <- embedImpl ops OpenAIEmbeddingOps
            { model = name
            , input = texts
            , encoding_format = "float" }
        trigger $ OnEmbeddingsReceived texts embeddings
        pure embeddings

    where
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