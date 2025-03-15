{-# LANGUAGE DeriveAnyClass #-}

module Heco.Effectful.Ego.Heco where

import Heco.Data.Default ()
import Heco.Data.Model (ModelName)
import Heco.Data.Embedding (Embedding(..))
import Heco.Data.Entity (EntityId)
import Heco.Data.TimePhase (TimePhase(..), ImmanantContent(..), SenseData(..), Action(..))
import Heco.Data.Entity.TH (deriveEntity)
import Heco.Data.Collection (CollectionName)
import Heco.Data.Message (Message(..))
import Heco.Data.Role (Role(User, System))
import Heco.Data.EgoError (EgoError(..))
import Heco.Events.EgoEvent (EgoEvent(..))
import Heco.Effectful.Event (Event, trigger, runEvent)
import Heco.Effectful.DatabaseService (DatabaseService(..), searchEntities, SearchOps(..), searchOps, loadCollection)
import Heco.Effectful.LanguageService (LanguageService(..), embed, chat, ChatOps)
import Heco.Effectful.InternalTimeStream (InternalTimeStream, getUrimpression, getRetention, enrichUrimpression_, progressUrimpression_)
import Heco.Effectful.Ego (Ego(..))

import Effectful (Eff, (:>), MonadIO (liftIO), IOE)
import Effectful.Dispatch.Dynamic (interpret, localSeqUnlift, HasCallStack)
import Effectful.Error.Dynamic (Error, throwError, runError, CallStack, catchError)
import Effectful.Exception (finally, catch, Exception(displayException), SomeException)

import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Data.Aeson ((.=))
import Data.Aeson.Encoding (Encoding, list, pairs, encodingToLazyByteString)
import Data.Vector qualified as V
import Data.Vector.Algorithms qualified as V
import Data.Vector.Unboxing qualified as VU
import Data.Default (Default)
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)

import GHC.Generics (Generic)
import Control.Monad (when)
import Heco.Data.LanguageError (LanguageError)

data HecoOps = HecoOps
    { characterPrompt :: Text
    , interactionMainPrompt :: Text
    , retentionPrompt :: Text
    , urimpressionPrompt :: Text
    , toolsPrompt :: Text 
    , chatOps :: ChatOps
    , memoryCollection :: CollectionName
    , memorySearchLimit :: Maybe Int
    , memoryEmbeddingModel :: ModelName }

data MemoryData = MemoryData
    { _type :: Text
    , content :: Text }

data MemoryEntity = MemoryEntity
    { id :: Maybe EntityId
    , vector :: Maybe (VU.Vector Float)
    , create_time :: Maybe Int
    , _type :: Text
    , content :: Text }
    deriving (Show, Generic, Default)

instance Eq MemoryEntity where
    a == b = a.id == b.id

instance Ord MemoryEntity where
    compare a b = compare (fromMaybe minBound a.id) (fromMaybe minBound b.id)

deriveEntity ''MemoryEntity

immanantContentToMemoryData :: ImmanantContent -> MemoryData
immanantContentToMemoryData = \case
    SenseDataContent s -> encodeSenseData s
    ActionContent a -> encodeAction a
    where
        encodeSenseData = \case
            VisualSenseData c -> toData "visual_data" c 
            AcousticSenseData c -> toData "acoustic_data" c
            OlfactorySenseData c -> toData "olfactory_data" c
            TactileSenseData c -> toData "tactile_data" c
            KinaestheticSenseData c -> toData "kinaesthetic_data" c
        encodeAction = \case
            StatementAction c -> toData "statement_action" c
            ReasoningAction c -> toData "reasoning_action" c
            QueryAction c -> toData "query_action" c
            WishAction c -> toData "wish_action" c
            WillAction c -> toData "will_action" c
            ImaginativeAction c -> toData "imaginative_action" c
            MemoryAction c -> toData "memory_action" c
        toData contentType content = MemoryData
            { _type = contentType
            , content = content }

embedMemoryData ::
    (HasCallStack, LanguageService :> es)
    => HecoOps -> MemoryData -> Eff es Embedding
embedMemoryData ops ent = do
    embed ops.memoryEmbeddingModel ent.content

createCharacterSection ::
    HecoOps -> Eff es TLB.Builder
createCharacterSection ops = 
    pure $ TLB.fromText ops.characterPrompt

encodeImmanantContent :: ImmanantContent -> Encoding
encodeImmanantContent c = 
    let e = immanantContentToMemoryData c
    in pairs $ "type" .= e._type <> "content" .= e.content

encodeTimePhase :: TimePhase -> Encoding
encodeTimePhase (TimePhase contents) = list encodeImmanantContent $ V.toList contents

encodingToLazyText :: Encoding -> TL.Text
encodingToLazyText = TL.decodeUtf8 . encodingToLazyByteString

injectMemoryContents ::
    ( HasCallStack
    , DatabaseService :> es
    , LanguageService :> es
    , InternalTimeStream :> es )
    => HecoOps -> Eff es ()
injectMemoryContents ops = do
    TimePhase contents <- getUrimpression
    when (V.length contents /= 0) do
        loadCollection ops.memoryCollection

        embeddings <- traverse (embedMemoryData ops . immanantContentToMemoryData) contents
        memEnts <- searchEntities @MemoryEntity ops.memoryCollection
            $ (searchOps . V.map coerce $ embeddings) { limit = ops.memorySearchLimit }

        let nubbedEnts = V.nubBy (\a b -> compare a.id b.id) memEnts
        enrichUrimpression_ $ V.map toImmanantContent nubbedEnts
    where
        toImmanantContent ent =
            ActionContent $ MemoryAction ent.content

createRetentionSection ::
    ( HasCallStack
    , InternalTimeStream :> es )
    => HecoOps -> Eff es TLB.Builder
createRetentionSection ops = do
    retention <- getRetention
    if V.length retention == 0
        then mempty
        else let encoding = list encodeTimePhase $ V.toList retention
                 json = encodingToLazyText encoding
            in pure $ TLB.fromText ops.retentionPrompt <> TLB.fromLazyText json

createUrimpressionSection ::
    ( HasCallStack
    , InternalTimeStream :> es )
    => HecoOps -> Eff es TLB.Builder
createUrimpressionSection ops = do
    urimpression <- getUrimpression
    case urimpression of
        TimePhase v | V.length v == 0 -> mempty
        _ -> let encoding = encodeTimePhase urimpression
                 json = encodingToLazyText encoding
            in pure $ TLB.fromText ops.urimpressionPrompt <> TLB.fromLazyText json

createTaskMessage ::
    ( HasCallStack
    , InternalTimeStream :> es )
    => HecoOps -> Eff es Message
createTaskMessage ops = do
    text <- mconcat . intersperse (TLB.fromText "\n\n") <$> sequenceA
        [ pure $ TLB.fromText ops.interactionMainPrompt
        , createRetentionSection ops
        , createUrimpressionSection ops ]
    pure $ Message
        { role = User
        , content = TL.toStrict $ TLB.toLazyText text }

runHecoEgo :: forall es a.
    ( HasCallStack
    , IOE :> es
    , DatabaseService :> es
    , LanguageService :> es
    , InternalTimeStream :> es
    , Event EgoEvent :> es
    , Error EgoError :> es
    , Error LanguageError :> es )
    => HecoOps
    -> Eff (Ego : es) a
    -> Eff es a
runHecoEgo ops = interpret \env -> \case
    InteractEgo eff ->
        (startInteraction
            >> localSeqUnlift env \unlift ->
                finally (unlift eff) $ finalizeInteraction)
        `catch` (\(e :: SomeException) -> throwError . UnhandledEgoError $ displayException e)
    where
        characterPromptMsg = Message
            { role = System
            , content = ops.characterPrompt }

        startInteraction = do
            progressUrimpression_
            trigger $ OnEgoInteractionStarted

        finalizeInteraction = do
            injectMemoryContents ops

            taskMsg <- createTaskMessage ops
            trigger $ OnEgoTaskGenerated taskMsg.content
            progressUrimpression_

            let doChat = chat ops.chatOps $ characterPromptMsg :| [taskMsg]
            reply <- doChat `catchError` (\_ (e :: LanguageError) -> do
                liftIO $ putStrLn $ displayException e
                liftIO $ putStrLn "Error found, retrying..."
                doChat)
            trigger $ OnEgoTaskResponded reply.content
            enrichUrimpression_ . V.singleton . ActionContent $ StatementAction reply.content

            injectMemoryContents ops
            progressUrimpression_
            trigger $ OnEgoInteractionCompleted

runHecoEgoEx ::
    ( HasCallStack
    , IOE :> es
    , DatabaseService :> es
    , LanguageService :> es
    , InternalTimeStream :> es
    , Error LanguageError :> es )
    => HecoOps
    -> Eff (Ego : Event EgoEvent : Error EgoError : es) a
    -> Eff es (Either (CallStack, EgoError) a)
runHecoEgoEx ops = 
    runError. runEvent . runHecoEgo ops