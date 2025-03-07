module Heco.Effectful.LanguageService.Ollama
    ( -- * Data types
      OllamaOps(..)
      -- * Language API
    , runOllamaLanguageService
    , runOllamaLanguageServiceEx
    ) where

import Heco.Data.Aeson ()
import Heco.Data.Model (ModelName(ModelName))
import Heco.Data.Role (Role(..))
import Heco.Data.Message (Message(..))
import Heco.Data.LanguageError (LanguageError(LanguageBackendError))
import Heco.Data.Embedding (Embedding(Embedding))
import Heco.Events.LanguageEvent (LanguageEvent(..))
import Heco.Effectful.Event (Event, trigger, runEvent)
import Heco.Effectful.LanguageService (LanguageService(..))
import Heco.Network.HTTP.Client (httpPost)

import Effectful (Eff, (:>), IOE, MonadIO (liftIO))
import Effectful.Exception (catchIO)
import Effectful.Dispatch.Dynamic (localSeqUnliftIO, localSeqLend, reinterpret)
import Effectful.Error.Dynamic (Error, HasCallStack, throwError, runError, CallStack)
import Effectful.Reader.Static (runReader, ask)

import Network.HTTP.Client
    ( responseTimeoutMicro,
      brRead,
      withResponse,
      defaultManagerSettings,
      newManager,
      ManagerSettings(managerResponseTimeout),
      Response(responseBody), httpLbs )

import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Data.Vector.Unboxing qualified as VU
import Data.Maybe (fromJust)
import Data.List.NonEmpty (NonEmpty)
import Data.Default (Default(..))

import Data.ByteString qualified as BS
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy qualified as BSL

import Data.Aeson (ToJSON(..), KeyValue(..))
import Data.Aeson qualified as Aeson
import Data.Aeson.TH (deriveToJSON, deriveFromJSON)

import Control.Exception (SomeException, catch)
import GHC.Generics (Generic)

data OllamaOps = OllamaOps
    { hostUrl :: Text
    , timeout :: Int }

instance Default OllamaOps where
    def = OllamaOps
        { hostUrl = "http://127.0.0.1:11434"
        , timeout = 60 }

data ChatOps = ChatOps
    { model :: Text
    , messages :: NonEmpty Message
    , tools :: Maybe Text }

instance ToJSON ChatOps where
    toJSON (ChatOps model messages tools) = Aeson.object
        [ "model" .= model
        , "messages" .= messages
        , "tools" .= tools
        , "stream" .= True ]

data ChatResp = ChatResp
    { message :: Maybe Message
    , done :: Bool }

deriveFromJSON Aeson.defaultOptions ''ChatResp

data EmbeddingOps = EmbeddingOps
    { model :: Text
    , input :: [Text] }
    deriving (Show, Eq, Generic)

deriveToJSON Aeson.defaultOptions ''EmbeddingOps

data EmbeddingResp = EmbeddingResp
    { embeddings :: [VU.Vector Float] }
    deriving (Show, Eq, Generic)

deriveFromJSON Aeson.defaultOptions ''EmbeddingResp

runOllamaLanguageService ::
    (HasCallStack, IOE :> es, Event LanguageEvent :> es, Error LanguageError :> es)
    => OllamaOps
    -> Eff (LanguageService : es) a
    -> Eff es a
runOllamaLanguageService ops = reinterpret evalServiceState \env -> \case
    Chat token (ModelName model) messages -> do
        manager <- ask
        res <- unliftEventIO env \unlift -> do
            req <- httpPost (ops.hostUrl <> "/api/chat") []
                $ ChatOps
                    { model = model
                    , messages = messages
                    , tools = Nothing }

            let builderToText = T.decodeUtf8 . BS.toStrict . toLazyByteString
                streamResponse builder response = do
                    bs <- brRead $ responseBody response
                    if BS.null bs
                        then pure . Right . builderToText $ builder
                        else case Aeson.eitherDecode @ChatResp (BSL.fromStrict bs) of
                            Left e -> pure $ Left e
                            Right r -> do
                                case r.message of
                                    Nothing -> pure ()
                                    Just msg -> unlift $ trigger $ OnLanguageChunkReceived token msg
                                let builder' = builder <> T.encodeUtf8Builder (fromJust r.message).content
                                if r.done
                                    then pure . Right . builderToText $ builder'
                                    else streamResponse builder' response

            withResponse req manager (streamResponse mempty)
                `catch` \(e :: SomeException) -> pure . Left $ "HTTP error occured: " ++ show e
        
        case res of
            Left e -> throwError $ LanguageBackendError e
            Right r -> do
                let msg = Message Assistant r
                trigger $ OnLanguageResponseReceived token msg
                pure msg

    Embed token (ModelName model) texts -> do
        manager <- ask
        resp <- safeLiftIO do
            req <- httpPost (ops.hostUrl <> "/api/embed") []
                $ EmbeddingOps
                    { model = model
                    , input = texts }
            responseBody <$> httpLbs req manager
        
        case Aeson.eitherDecode @EmbeddingResp resp of
            Left e -> throwError $ LanguageBackendError e
            Right r -> case r.embeddings of
                [] -> throwError $ LanguageBackendError "embedding not received"
                vectors -> do
                    let embeddings = map Embedding vectors
                    trigger $ OnEmbeddingsReceived token texts embeddings
                    pure embeddings
    where
        evalServiceState e = do
            manager <- liftIO $ newManager defaultManagerSettings
                { managerResponseTimeout =
                    responseTimeoutMicro $ ops.timeout * 1000000 }
            runReader manager e

        unliftEventIO env f = localSeqLend @'[Event LanguageEvent] env \useEvent ->
            (localSeqUnliftIO env \unlift -> f $ unlift . useEvent)
                `catchIO` (throwError . LanguageBackendError . show)

        safeLiftIO m = liftIO m 
            `catchIO` (throwError . LanguageBackendError . show)
    
runOllamaLanguageServiceEx ::
    (HasCallStack, IOE :> es)
    => OllamaOps
    -> Eff (LanguageService : Event LanguageEvent : Error LanguageError : es) a
    -> Eff es (Either (CallStack, LanguageError) a)
runOllamaLanguageServiceEx ops = 
    runError . runEvent . runOllamaLanguageService ops