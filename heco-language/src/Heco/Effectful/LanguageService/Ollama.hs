module Heco.Effectful.LanguageService.Ollama
    ( -- * Data types
      OllamaOps(..)
    , ollamaOps
      -- * Language API
    , runOllamaLanguageService
    , runOllamaLanguageServiceEx
    ) where

import Heco.Network.HTTP.Client (httpPost)
import Heco.Data.Aeson (defaultAesonOps)
import Heco.Data.Model (ModelName(..))
import Heco.Data.Role (Role(..))
import Heco.Data.Message (Message(..), ToolCall(..), newAssistantMessage)
import Heco.Data.LanguageError (LanguageError(LanguageBackendError, LanguageInputError))
import Heco.Data.Embedding (Embedding(Embedding))
import Heco.Data.FunctionSchema (FunctionSchema(..))
import Heco.Events.LanguageEvent (LanguageEvent(..))
import Heco.Effectful.HTTP (evalHttpManager)
import Heco.Effectful.Event (Event, trigger, runEvent)
import Heco.Effectful.LanguageService (LanguageService(..), ChatOps(..))
import Heco.Effectful.LanguageService.Common (unliftEventIO, relayError)

import Effectful (Eff, (:>), IOE, MonadIO (liftIO))
import Effectful.Dispatch.Dynamic (reinterpret)
import Effectful.Error.Dynamic (Error, HasCallStack, throwError, runError, CallStack)
import Effectful.Reader.Static (ask, Reader)

import Network.HTTP.Client
    ( brRead,
      withResponse,
      Response(responseBody), httpLbs, Manager )

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Unboxing qualified as VU
import Data.Coerce (coerce)
import Data.HashMap.Strict (HashMap)

import Data.ByteString qualified as BS
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy qualified as BSL

import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.Aeson.TH (deriveToJSON, deriveFromJSON, deriveJSON)

import Control.Exception (throw)
import Control.Monad.Extra (when)
import Pattern.Cast (cast)

data OllamaOps = OllamaOps
    { url :: Text
    , timeout :: Maybe Int }

ollamaOps :: Text -> OllamaOps
ollamaOps url = OllamaOps
    { url = url
    , timeout = Nothing }

data OllamaFunctionToolCall = OllamaFunctionToolCall
    { name :: Text
    , arguments :: Maybe (HashMap Text Value) }

data OllamaToolCall = OllamaToolCall
    { function :: OllamaFunctionToolCall }

data OllamaMessage = OllamaMessage
    { role :: Role
    , content :: Text
    , tool_calls :: Maybe [OllamaToolCall] }

toOllamaToolCall :: ToolCall -> OllamaToolCall
toOllamaToolCall t = OllamaToolCall
    { function = OllamaFunctionToolCall
        { name = t.name
        , arguments = Just t.arguments } }

toOllamaMessage :: Message -> OllamaMessage
toOllamaMessage = \case
    SystemMessage _ t -> OllamaMessage System t Nothing
    UserMessage _ t -> OllamaMessage User t Nothing
    ToolMessage _ _ -> OllamaMessage Tool "" Nothing
    AssistantMessage _ t toolCalls ->
        OllamaMessage Assistant t $ Just $ map toOllamaToolCall toolCalls

data OllamaChatOps = OllamaChatOps
    { model :: Text
    , messages :: Vector OllamaMessage
    , tools :: [FunctionSchema]
    , stream :: Bool }

data OllamaChatResp = OllamaChatResp
    { message :: OllamaMessage
    , done :: Bool }

deriveJSON defaultAesonOps ''OllamaFunctionToolCall
deriveJSON defaultAesonOps ''OllamaToolCall
deriveJSON defaultAesonOps ''OllamaMessage
deriveToJSON defaultAesonOps ''OllamaChatOps
deriveFromJSON defaultAesonOps ''OllamaChatResp

data OllamaEmbeddingOps = OllamaEmbeddingOps
    { model :: Text
    , input :: Vector Text }

data OllamaEmbeddingResp = OllamaEmbeddingResp
    { embeddings :: Vector (VU.Vector Float) }

deriveToJSON defaultAesonOps ''OllamaEmbeddingOps
deriveFromJSON defaultAesonOps ''OllamaEmbeddingResp

embedImpl ::
    ( HasCallStack
    , IOE :> es
    , Error LanguageError :> es
    , Reader Manager :> es )
    => OllamaOps -> OllamaEmbeddingOps -> Eff es (Vector Embedding)
embedImpl ops req = do
    manager <- ask
    embeddingsRaw <-
        (liftIO $ relayError do
            req' <- httpPost (ops.url <> "/api/embed") [] req
            responseBody <$> httpLbs req' manager)
        >>= either throwError pure

    case Aeson.eitherDecode @OllamaEmbeddingResp embeddingsRaw of
        Left e -> throwError $ LanguageBackendError e
        Right (OllamaEmbeddingResp { embeddings = embeddings }) ->
            if V.length embeddings == 0
                then throwError $ LanguageBackendError "embedding not received"
                else pure $ coerce embeddings

runOllamaLanguageService ::
    (HasCallStack, IOE :> es, Event LanguageEvent :> es, Error LanguageError :> es)
    => OllamaOps
    -> Eff (LanguageService : es) a
    -> Eff es a
runOllamaLanguageService ops = reinterpret (evalHttpManager ops.timeout) \env -> \case
    Chat chatOps messages -> do
        manager <- ask
        resp <- unliftEventIO env \unlift -> do
            req <- httpPost (ops.url <> "/api/chat") []
                OllamaChatOps
                    { model = cast chatOps.modelName
                    , messages = V.map toOllamaMessage messages
                    , tools = chatOps.tools
                    , stream = True }

            let builderToText = T.decodeUtf8 . BS.toStrict . toLazyByteString
                streamResponse builder response = do
                    bs <- brRead $ responseBody response
                    if BS.null bs
                        then pure . builderToText $ builder
                        else case Aeson.eitherDecode @OllamaChatResp (BSL.fromStrict bs) of
                            Left e -> throw $ LanguageBackendError e
                            Right r -> do
                                let content = r.message.content
                                    builder' = builder <> T.encodeUtf8Builder content
                                when (T.length content /= 0) $
                                    unlift . trigger . OnUtteranceChunkReceived $ content
                                if r.done
                                    then pure . builderToText $ builder'
                                    else streamResponse builder' response

            withResponse req manager (streamResponse mempty)

        msg <- newAssistantMessage resp []
        trigger $ OnMessageReceived msg
        pure msg

    Embed (ModelName name) text -> do
        let vec = V.singleton text
        embeddings <- embedImpl ops OllamaEmbeddingOps
            { model = name
            , input = vec }
        trigger $ OnEmbeddingsReceived vec embeddings
        pure $ embeddings V.! 0

    EmbedMany (ModelName name) texts -> do
        when (V.length texts == 0) $
            throwError $ LanguageInputError "texts cannot be empty"
        embeddings <- embedImpl ops OllamaEmbeddingOps
            { model = name
            , input = texts }
        trigger $ OnEmbeddingsReceived texts embeddings
        pure embeddings

runOllamaLanguageServiceEx ::
    (HasCallStack, IOE :> es)
    => OllamaOps
    -> Eff (LanguageService : Event LanguageEvent : Error LanguageError : es) a
    -> Eff es (Either (CallStack, LanguageError) a)
runOllamaLanguageServiceEx ops = 
    runError . runEvent . runOllamaLanguageService ops