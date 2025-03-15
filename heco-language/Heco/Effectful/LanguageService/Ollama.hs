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
import Heco.Data.Message (Message(..))
import Heco.Data.LanguageError (LanguageError(LanguageBackendError))
import Heco.Data.Embedding (Embedding(Embedding))
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
import Data.Text.Encoding qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Unboxing qualified as VU
import Data.Maybe (fromJust)
import Data.List.NonEmpty (NonEmpty)
import Data.Coerce (coerce)

import Data.ByteString qualified as BS
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy qualified as BSL

import Data.Aeson qualified as Aeson
import Data.Aeson.TH (deriveToJSON, deriveFromJSON)

import Control.Exception (throw)
import Control.Monad.Extra (whenJust)
import Pattern.Cast (cast)

data OllamaOps = OllamaOps
    { url :: Text
    , timeout :: Maybe Int }

ollamaOps :: Text -> OllamaOps
ollamaOps url = OllamaOps
    { url = url
    , timeout = Nothing }

data OllamaChatOps = OllamaChatOps
    { model :: Text
    , messages :: NonEmpty Message
    , stream :: Bool }

data OllamaChatResp = OllamaChatResp
    { message :: Maybe Message
    , done :: Bool }

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
                else pure (coerce embeddings :: Vector Embedding)

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
                $ OllamaChatOps
                    { model = cast chatOps.modelName
                    , messages = messages
                    , stream = True }

            let builderToText = T.decodeUtf8 . BS.toStrict . toLazyByteString
                streamResponse builder response = do
                    bs <- brRead $ responseBody response
                    if BS.null bs
                        then pure . builderToText $ builder
                        else case Aeson.eitherDecode @OllamaChatResp (BSL.fromStrict bs) of
                            Left e -> throw $ LanguageBackendError e
                            Right r -> do
                                whenJust r.message $ unlift . trigger . OnDiscourseChunkReceived
                                let builder' = builder
                                        <> T.encodeUtf8Builder (fromJust r.message).content
                                if r.done
                                    then pure . builderToText $ builder'
                                    else streamResponse builder' response

            withResponse req manager (streamResponse mempty)

        let msg = Message Assistant resp
        trigger $ OnDiscourseResponseReceived msg
        pure msg

    Embed (ModelName name) text -> do
        let vec = V.singleton text
        embeddings <- embedImpl ops OllamaEmbeddingOps
            { model = name
            , input = vec }
        trigger $ OnEmbeddingsReceived vec embeddings
        pure $ embeddings V.! 0
    
    EmbedMany (ModelName name) texts -> do
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