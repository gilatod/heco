{-# LANGUAGE DeriveAnyClass #-}

module Heco.Effectful.Ego.Heco where

import Heco.Data.Default ()
import Heco.Data.Aeson (encodingToLazyText)
import Heco.Data.Model (ModelName)
import Heco.Data.Embedding (Embedding(..))
import Heco.Data.Entity (EntityId(EntityId))
import Heco.Data.Memory (Memory(..))
import Heco.Data.Noema (Noema(..), NoemaId (NoemaId), NoemaCategory (NoemaCategory))
import Heco.Data.TimePhase (TimePhase(..), ImmanantContent(..), SenseData(..), Action(..))
import Heco.Data.Entity.TH (deriveEntity)
import Heco.Data.Collection (CollectionName)
import Heco.Data.Message (Message(..))
import Heco.Data.Role (Role(User, System))
import Heco.Data.LanguageError (LanguageError)
import Heco.Data.EgoError (EgoError(..))
import Heco.Events.EgoEvent (EgoEvent(..))
import Heco.Events.InternalTimeStreamEvent (InternalTimeStreamEvent(OnTimePhaseLost))
import Heco.Effectful.Event (Event, trigger, runEvent, on)
import Heco.Effectful.DatabaseService
    ( SearchOps(limit),
      DatabaseService,
      searchOps,
      searchEntities,
      loadCollection,
      addEntity,
      deleteEntities,
      getEntity,
      addEntities_,
      setEntity_,
      setEntities_ )
import Heco.Effectful.LanguageService
    ( chat, embed, embedMany, ChatOps, LanguageService )
import Heco.Effectful.InternalTimeStream
    ( InternalTimeStream,
      enrichUrimpressionSingular_,
      enrichUrimpression_,
      getRetention,
      getUrimpression,
      progressUrimpression_ )
import Heco.Effectful.Ego (Ego(..))

import Effectful (Eff, (:>), MonadIO (liftIO), IOE)
import Effectful.Dispatch.Dynamic (interpret, localSeqUnlift, HasCallStack, LocalEnv)
import Effectful.Error.Dynamic (Error, throwError, runError, CallStack, catchError)
import Effectful.Exception (finally, catch, Exception(displayException), SomeException)

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Data.Text.Lazy.Builder.Int qualified as TLB
import Data.Aeson ((.=))
import Data.Aeson.Encoding (Encoding, list, pairs)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Algorithms qualified as V
import Data.Vector.Unboxing qualified as VU
import Data.Default (Default)
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe, isJust, fromJust)
import Data.Time (getCurrentTime, UTCTime)
import Data.Function ((&))

import Control.Monad (when)
import GHC.Generics (Generic)
import GHC.Records (HasField)
import Pattern.Cast (Cast(cast))

data HecoOps = HecoOps
    { characterPrompt :: Text
    , interactionMainPrompt :: Text
    , retentionPrompt :: Text
    , associationPrompt :: Text
    , urimpressionPrompt :: Text
    , toolsPrompt :: Text 
    , chatOps :: ChatOps
    , memoryCollection :: CollectionName
    , memorySearchLimit :: Maybe Int
    , memoryEmbeddingModel :: ModelName }

type IsMemory mem =
    ( HasField "content" mem Text )

data MemoryEntity = MemoryEntity
    { id :: Maybe EntityId
    , vector :: Maybe (VU.Vector Float)
    , topic :: Text
    , content :: Text
    , time :: Maybe UTCTime }
    deriving (Show, Generic, Default)

instance Eq MemoryEntity where
    a == b = a.id == b.id

instance Ord MemoryEntity where
    compare a b = compare (fromMaybe minBound a.id) (fromMaybe minBound b.id)

deriveEntity ''MemoryEntity

splitTopic :: Text -> [Text]
splitTopic = T.split \c -> c == '/'

joinTopic :: [Text] -> Text
joinTopic = \case
    [t] -> t
    ts -> T.concat . map (`T.snoc` '/') $ ts

immanantContentToMemory :: ImmanantContent -> Memory
immanantContentToMemory = \case
    SenseDataContent s -> encodeSenseData s
    ActionContent a -> encodeAction a
    NoemaContent n -> encodeNoema n
    where
        encodeSenseData = \case
            VisualSenseData c -> toData "visual" c 
            AcousticSenseData c -> toData "acoustic" c
            OlfactorySenseData c -> toData "olfactory" c
            TactileSenseData c -> toData "tactile_data" c
            KinaestheticSenseData c -> toData "kinaesthetic_data" c

        encodeAction = \case
            StatementAction c -> toData "assistant" c
            ReasoningAction c -> toData "think" c
            QueryAction c -> toData "query" c
            WishAction c -> toData "wish" c
            WillAction c -> toData "will" c
            ImaginativeAction c -> toData "imagine" c
            MemoryAction mem -> mem

        encodeNoema n = Memory
            { topic = ["object", cast n.category]
            , baseId = Just $ cast n.id
            , content = n.content
            , time = Nothing }

        toData contentType content = Memory
            { topic = [contentType]
            , baseId = Nothing
            , content = content
            , time = Nothing }

embedMemory ::
    (HasCallStack, IsMemory mem, LanguageService :> es)
    => HecoOps -> mem -> Eff es Embedding
embedMemory ops ent = do
    embed ops.memoryEmbeddingModel ent.content

createCharacterSection ::
    HecoOps -> Eff es TLB.Builder
createCharacterSection ops = 
    pure $ TLB.fromText ops.characterPrompt

encodeImmanantContent :: ImmanantContent -> Encoding
encodeImmanantContent c = 
    let e = immanantContentToMemory c
    in pairs $ "topic" .= e.topic <> "content" .= e.content
            -- <> maybe mempty (\t -> "time" .= t) e.time

encodeTimePhase :: TimePhase -> Encoding
encodeTimePhase (TimePhase contents) = list encodeImmanantContent $ V.toList contents

associateMemory ::
    ( HasCallStack
    , DatabaseService :> es
    , LanguageService :> es
    , InternalTimeStream :> es )
    => HecoOps -> Eff es (Vector ImmanantContent)
associateMemory ops = do
    TimePhase contents <- getUrimpression
    if V.length contents == 0
        then pure mempty
        else do
            loadCollection ops.memoryCollection

            embeddings <- traverse (embedMemory ops . immanantContentToMemory) contents
            memEnts <- searchEntities @MemoryEntity ops.memoryCollection $
                (searchOps . V.map coerce $ embeddings) { limit = ops.memorySearchLimit }

            let nubbedEnts = V.nubBy (\a b -> compare a.id b.id) memEnts
            pure $ V.map toImmanantContent nubbedEnts
    where
        toImmanantContent ent =
            ActionContent $ MemoryAction $ Memory
                { topic = "memory" : splitTopic ent.topic
                , baseId = cast <$> ent.id
                , content = ent.content
                , time = ent.time }

injectMemory ::
    ( HasCallStack
    , DatabaseService :> es
    , LanguageService :> es
    , InternalTimeStream :> es )
    => HecoOps -> Eff es ()
injectMemory ops = associateMemory ops >>= enrichUrimpression_

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

createAssociationSection ::
    HecoOps -> Vector ImmanantContent -> Eff es TLB.Builder
createAssociationSection ops contents = do
    if V.length contents == 0
        then pure mempty
        else let encoding = list encodeImmanantContent $ V.toList contents
                 json = encodingToLazyText encoding
            in pure $ TLB.fromText ops.associationPrompt <> TLB.fromLazyText json

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
    => HecoOps -> Vector ImmanantContent -> Eff es Message
createTaskMessage ops associations = do
    text <- mconcat . intersperse (TLB.fromText "\n\n") <$> sequenceA
        [ pure $ TLB.fromText ops.interactionMainPrompt
        , createAssociationSection ops associations
        , createRetentionSection ops
        , createUrimpressionSection ops ]
    pure $ Message
        { role = User
        , content = TL.toStrict $ TLB.toLazyText text }

memorize ::
    ( HasCallStack
    , IOE :> es
    , DatabaseService :> es
    , LanguageService :> es )
    => HecoOps -> Vector Memory -> Eff es ()
memorize ops mems = do
    time <- liftIO getCurrentTime
    embeddings <- embedMany ops.memoryEmbeddingModel $ V.map (\m -> m.content) mems

    let ents = embeddings & V.imap \i embedding ->
            let mem = mems V.! i
            in MemoryEntity
                { id = EntityId <$> mem.baseId
                , vector = Just $ cast embedding
                , time = Just time
                , topic = joinTopic
                    case mem.topic of
                        "memory":g -> g
                        otherwise -> otherwise
                , content = mem.content }
        (extEnts, newEnts) = V.unstablePartition (\ent -> isJust ent.id) ents

    when (V.length extEnts /= 0) $ setEntities_ ops.memoryCollection extEnts
    when (V.length newEnts /= 0) $ addEntities_ ops.memoryCollection newEnts

memorizeTimePhase ::
    ( HasCallStack
    , IOE :> es
    , DatabaseService :> es
    , LanguageService :> es )
    => HecoOps -> TimePhase -> Eff es ()
memorizeTimePhase ops (TimePhase contents) = do
    let mems = V.map immanantContentToMemory contents
    if V.length mems == 0
        then pure ()
        else memorize ops mems

wrapInteraction ::
    ( HasCallStack
    , IOE :> es
    , DatabaseService :> es
    , LanguageService :> es
    , InternalTimeStream :> es
    , Event EgoEvent :> es
    , Error LanguageError :> es )
    => HecoOps -> LocalEnv localEs es -> Eff localEs a -> Eff es a
wrapInteraction ops env eff = 
    startInteraction
        >> localSeqUnlift env \unlift ->
            finally (unlift eff) $ finalizeInteraction
    where
        characterPromptMsg = Message
            { role = System
            , content = ops.characterPrompt }

        startInteraction = do
            progressUrimpression_
            trigger $ OnEgoInteractionStarted

        finalizeInteraction = do
            mem <- associateMemory ops
            taskMsg <- createTaskMessage ops mem
            trigger $ OnEgoTaskGenerated taskMsg.content

            --enrichUrimpression_ mem
            progressUrimpression_

            let doChat = chat ops.chatOps $ characterPromptMsg :| [taskMsg]
                chatLoop = do
                    res <- (Right <$> doChat) `catchError` (\_ (e :: LanguageError) -> pure $ Left e)
                    case res of
                        Left e -> do
                            liftIO $ putStrLn $ displayException e
                            liftIO $ putStrLn "Error found, retrying..."
                            chatLoop
                        Right r -> pure r
            reply <- chatLoop
            trigger $ OnEgoTaskResponded reply.content

            enrichUrimpression_ . V.singleton $
                ActionContent $ StatementAction reply.content
            --injectMemory ops

            progressUrimpression_
            trigger $ OnEgoInteractionCompleted

runHecoEgo :: forall es a.
    ( HasCallStack
    , IOE :> es
    , DatabaseService :> es
    , LanguageService :> es
    , InternalTimeStream :> es
    , Event EgoEvent :> es
    , Event InternalTimeStreamEvent :> es
    , Error EgoError :> es
    , Error LanguageError :> es )
    => HecoOps -> Eff (Ego : es) a -> Eff es a
runHecoEgo ops = interpret \env -> \case
    InteractEgo eff -> wrapInteraction ops env eff
        `on` \case
            OnTimePhaseLost tp -> memorizeTimePhase ops tp
            _ -> pure ()
        `catch` \(e :: SomeException) ->
            (throwError . UnhandledEgoError $ displayException e)

    InjectMemory -> injectMemory ops

    PresentiateNoema noema -> do
        enrichUrimpressionSingular_ $ NoemaContent noema

    CreateNoema category content -> do
        time <- liftIO $ getCurrentTime
        Embedding embedding <- embed ops.memoryEmbeddingModel content
        EntityId id <- addEntity ops.memoryCollection MemoryEntity
            { id = Nothing
            , vector = Just embedding
            , topic = "object/" <> cast category
            , content = content
            , time = Just time }
        pure Noema
            { id = NoemaId id
            , category = category
            , content = "" }

    SetNoema noema -> do
        time <- liftIO $ getCurrentTime
        Embedding embedding <- embed ops.memoryEmbeddingModel noema.content
        setEntity_ ops.memoryCollection MemoryEntity
            { id = Just . EntityId $ cast noema.id
            , vector = Just embedding
            , topic = "object/" <> cast noema.category
            , content = noema.content
            , time = Just time }
 
    GetNoema nid@(NoemaId id) -> do
        ent <- getEntity @MemoryEntity ops.memoryCollection (EntityId id)
        case splitTopic ent.topic of
            ["object", category] ->
                pure Noema
                    { id = nid
                    , category = NoemaCategory category
                    , content = ent.content }
            _ -> throwError $ EgoInvalidNoemaError $ "invalid noema with id " <> show id

    FindNoemata (Embedding embedding) -> do
        ents <- searchEntities @MemoryEntity ops.memoryCollection $
            (searchOps $ V.singleton embedding) { limit = ops.memorySearchLimit  }
        
        pure $ ents & V.mapMaybe \ent ->
            case splitTopic ent.topic of
                ["object", category] ->
                    let id = NoemaId . cast $ fromJust ent.id
                    in Just Noema
                        { id = id
                        , category = NoemaCategory category
                        , content = ent.content }
                _ -> Nothing
    
    DeleteNoema (NoemaId id) -> do
        let idText = TL.toStrict . TLB.toLazyText . TLB.decimal $ id
        deleteEntities ops.memoryCollection $ "id==" <> idText

runHecoEgoEx ::
    ( HasCallStack
    , IOE :> es
    , DatabaseService :> es
    , LanguageService :> es
    , InternalTimeStream :> es
    , Event InternalTimeStreamEvent :> es
    , Error LanguageError :> es )
    => HecoOps
    -> Eff (Ego : Event EgoEvent : Error EgoError : es) a
    -> Eff es (Either (CallStack, EgoError) a)
runHecoEgoEx ops = 
    runError. runEvent . runHecoEgo ops