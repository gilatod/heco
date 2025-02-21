module Heco.Effectful.LanguageService.Ollama
    ( -- * Data types
      OllamaModels(..)
    , OllamaOps(..)
    , defaultOllamaOps
      -- * Language API
    , runOllamaLanguageService
    , runOllamaLanguageServiceEx
    ) where

import Heco.Data.Role (Role(..))
import Heco.Data.Message (Message(..))
import Heco.Data.LanguageError (LanguageError(LanguageBackendError))
import Heco.Events.LanguageEvent (LanguageEvent(..))
import Heco.Effectful.Event (Event, trigger, runEvent)
import Heco.Effectful.LanguageService (LanguageService(..))

import Effectful (Eff, (:>), IOE, MonadIO (liftIO))
import Effectful.Exception (catchIO)
import Effectful.Dispatch.Dynamic (interpret, localSeqUnliftIO, localSeqLend)
import Effectful.Error.Dynamic (Error, HasCallStack, throwError, runErrorNoCallStack)

import Network.HTTP.Client
    ( responseTimeoutMicro,
      brRead,
      withResponse,
      defaultManagerSettings,
      newManager,
      parseRequest,
      ManagerSettings(managerResponseTimeout),
      Request(requestBody, method),
      RequestBody(RequestBodyLBS),
      Response(responseBody), httpLbs )

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.ByteString qualified as BS
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Aeson (encode, eitherDecode, object, FromJSON(..), ToJSON(..), KeyValue(..), FromJSON)
import Data.Maybe (fromJust)
import Control.Exception (SomeException, catch)
import GHC.Generics (Generic)
import Heco.Data.Embeddings (Embeddings(Embeddings))
import Data.List.NonEmpty (NonEmpty)

data OllamaModels = OllamaModels
    { chat :: Text
    , embeddings :: Text }

data OllamaOps = OllamaOps
    { models :: OllamaModels
    , hostUrl :: Text
    , responseTimeOut :: Int }

defaultOllamaOps :: OllamaOps
defaultOllamaOps = OllamaOps
    { models = OllamaModels
        { chat = ""
        , embeddings = "" }
    , hostUrl = "http://127.0.0.1:11434/"
    , responseTimeOut = 15 }

data ChatOps = ChatOps
    { model :: Text
    , messages :: NonEmpty Message
    , tools :: Maybe Text }

instance ToJSON ChatOps where
    toJSON (ChatOps model messages tools) = object
        [ "model" .= model
        , "messages" .= messages
        , "tools" .= tools
        , "stream" .= True ]

data ChatResp = ChatResp
    { model :: Text
    , message :: Maybe Message
    , done :: Bool }
    deriving (Generic)

instance FromJSON ChatResp
instance ToJSON ChatResp

data EmbeddingOps = EmbeddingOps
    { model :: Text
    , input :: Text }
    deriving (Show, Eq, Generic)

instance ToJSON EmbeddingOps

data EmbeddingResp = EmbeddingResp
    { embeddings :: [[Float]] }
    deriving (Show, Eq, Generic)

instance FromJSON EmbeddingResp

runOllamaLanguageService ::
    (HasCallStack, IOE :> es, Event LanguageEvent :> es, Error LanguageError :> es)
    => OllamaOps
    -> Eff (LanguageService : es) a
    -> Eff es a
runOllamaLanguageService ops = interpret \env -> \case
    Chat token messages -> do
        res <- unliftEventIO env \unlift -> do
            manager <- newManager defaultManagerSettings
                { managerResponseTimeout =
                    responseTimeoutMicro (ops.responseTimeOut * 60 * 1000000) }
            preReq <- parseRequest $ T.unpack $ ops.hostUrl <> "/api/chat"

            let reqBody = ChatOps
                    { model = ops.models.chat
                    , messages = messages
                    , tools = Nothing }
                req = preReq
                    { method = "POST"
                    , requestBody = RequestBodyLBS $ encode reqBody }
                builderToText = T.decodeUtf8 . BS.toStrict . toLazyByteString
                streamResponse builder response = do
                    bs <- brRead $ responseBody response
                    if BS.null bs
                        then pure . Right . builderToText $ builder
                        else case eitherDecode (BSL.fromStrict bs) :: Either String ChatResp of
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

    Embed token text -> do
        res <- safeLiftIO do
            manager <- newManager defaultManagerSettings
                { managerResponseTimeout =
                    responseTimeoutMicro (ops.responseTimeOut * 60 * 1000000) }
            prevReq <- parseRequest $ T.unpack $ ops.hostUrl <> "/api/embed"

            let reqBody = EmbeddingOps
                    { model = ops.models.embeddings
                    , input = text }
                req = prevReq
                    { method = "POST"
                    , requestBody = RequestBodyLBS $ encode reqBody }
            
            response <- httpLbs req manager
            pure $ eitherDecode (responseBody response) :: IO (Either String EmbeddingResp)
        
        case res of
            Left e -> throwError $ LanguageBackendError e
            Right r -> do
                let embeddings = Embeddings r.embeddings
                trigger $ OnEmbeddingsReceived token embeddings
                pure embeddings
    where
        unliftEventIO env f = localSeqLend @'[Event LanguageEvent] env \useEvent ->
            (localSeqUnliftIO env \unlift -> f $ unlift . useEvent)
                `catchIO` \e -> (throwError . LanguageBackendError . show $ e)
        safeLiftIO m = liftIO m 
            `catchIO` \e -> (throwError . LanguageBackendError . show $ e)
    
runOllamaLanguageServiceEx ::
    (HasCallStack, IOE :> es)
    => OllamaOps
    -> Eff (LanguageService : Event LanguageEvent : Error LanguageError : es) a
    -> Eff es (Either LanguageError a)
runOllamaLanguageServiceEx ops = 
    runErrorNoCallStack . runEvent . runOllamaLanguageService ops