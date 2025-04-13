{-# LANGUAGE DeriveAnyClass #-}

module Heco.Effectful.Ego.Heco where

import Heco.Data.Default ()
import Heco.Data.Model (ModelName)
import Heco.Data.Embedding (Embedding(..))
import Heco.Data.TimePhase
    ( TimePhase(..),
      ImmanantContent(..),
      AnyImmanantContent (AnyImmanantContent),
      joinImmanantContent )
import Heco.Data.Immanant.Memory (Memory(..), anyImmanantContentToMemory)
import Heco.Data.Immanant.Terminal (Terminal(..))
import Heco.Data.Collection (CollectionName)
import Heco.Data.Message (Message(..), newUserMessage, newSystemMessage, newToolMessage, ToolCall(..), ToolResponse(..))
import Heco.Data.LanguageError (LanguageError)
import Heco.Data.EgoError (EgoError(..))
import Heco.Events.EgoEvent (EgoEvent(..))
import Heco.Events.InternalTimeStreamEvent (InternalTimeStreamEvent(OnTimePhaseLost))
import Heco.Effectful.Event (Event, trigger, runEvent, on)
import Heco.Effectful.DatabaseService
    ( SearchOps(..),
      DatabaseService,
      searchEntities,
      loadCollection,
      addEntities_ )
import Heco.Effectful.LanguageService
    ( chat, embed, embedMany, ChatOps(..), LanguageService )
import Heco.Effectful.LanguageToolProvider (LanguageToolProvider, invokeLanguageTool, getLanguageTools)
import Heco.Effectful.InternalTimeStream
    ( InternalTimeStream,
      enrichUrimpression_,
      getRetention,
      getUrimpression,
      progressUrimpression_, enrichUrimpressionSingle_ )
import Heco.Effectful.Ego (Ego(..))

import Effectful (Eff, (:>), MonadIO (liftIO), IOE)
import Effectful.Dispatch.Dynamic (localSeqUnlift, HasCallStack, LocalEnv, reinterpret)
import Effectful.State.Static.Shared (State, modify, state, evalState)
import Effectful.Reader.Static (runReader, Reader, ask)
import Effectful.Error.Dynamic (Error, throwError, runError, CallStack, catchError)
import Effectful.Exception (finally, catch, Exception(displayException), SomeException)

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB

import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Algorithms qualified as V
import Data.Vector.Mutable qualified as VM

import Data.Default (def)
import Data.List (intersperse)
import Data.Coerce (coerce)
import Data.Maybe (isNothing)
import Data.Time (getCurrentTime)
import Data.Function ((&))
import Data.Cache.LRU (LRU)
import Data.Cache.LRU qualified as LRU
import Data.Unique (Unique)
import Data.Tuple (swap)
import Data.Typeable qualified as Typeable

import Control.Monad (when, forM)
import Pattern.Cast (Cast(cast))

data HecoMemoryOps = HecoMemoryOps
    { collectionName :: CollectionName
    , embeddingModel :: ModelName
    , searchOps :: SearchOps }

hecoMemoryOps :: CollectionName -> ModelName -> HecoMemoryOps
hecoMemoryOps c m = HecoMemoryOps
    { collectionName = c
    , embeddingModel = m
    , searchOps = def }

data HecoOps = HecoOps
    { characterPrompt :: Text
    , taskPrompt :: Text
    , chatOps :: ChatOps
    , memoryOps :: HecoMemoryOps
    , immanantContentFormatter :: AnyImmanantContent -> TLB.Builder
    , messageCacheLimit :: Maybe Int }

immanantContentXMLFormatter :: AnyImmanantContent -> TLB.Builder
immanantContentXMLFormatter (AnyImmanantContent c) =
    case encodeImmanantContent c of
        [] -> mempty
        [c] -> "<" <> TLB.fromText c <> xmlAttrs <> "/>"
        c:cs -> 
            let cb = TLB.fromText c
            in "<" <> cb <> xmlAttrs <> ">\n"
            <> mconcat (intersperse ":" $ map TLB.fromText cs)
            <> "\n</" <> cb <> ">"
    where
        xmlAttrs = case getImmanantContentAttributes c of
            [] -> mempty
            attrs -> attrs & mconcat . (" ":) . intersperse " " . map \(key, value) ->
                TLB.fromText key <> "=\"" <>
                TLB.fromText (T.replace "\"" "\\\"" value) <> "\""

embedImmanantContent ::
    (HasCallStack, LanguageService :> es)
    => HecoOps -> AnyImmanantContent -> Eff es Embedding
embedImmanantContent ops (AnyImmanantContent mem) = do
    embed ops.memoryOps.embeddingModel $ joinImmanantContent ":" mem

memorizeImmanantContents ::
    ( HasCallStack
    , IOE :> es
    , DatabaseService :> es
    , LanguageService :> es )
    => HecoOps -> Vector AnyImmanantContent -> Eff es ()
memorizeImmanantContents ops contents = do
    let mems = V.map anyImmanantContentToMemory contents
        newMems = V.filter (\ent -> isNothing ent.id) mems

    when (V.length newMems /= 0) do
        time <- liftIO getCurrentTime
        embeddings <- embedMany ops.memoryOps.embeddingModel $
            newMems & V.map \mem -> T.concat $ intersperse ":" mem.content
        addEntities_ ops.memoryOps.collectionName $
            newMems & V.imap \i mem -> mem
                { time = Just time
                , vector = Just $ coerce $ embeddings V.! i }

formatImmanantContents ::
    HecoOps -> Vector AnyImmanantContent -> TLB.Builder
formatImmanantContents ops contents = do
    if V.length contents == 0
        then mempty
        else V.foldl accumulate mempty $ V.reverse contents
    where
        accumulate acc c =
            if acc == mempty
                then ops.immanantContentFormatter c
                else acc <> "\n\n" <> ops.immanantContentFormatter c

timePhaseToMessasge ::
    ( HasCallStack
    , IOE :> es
    , State (LRU Unique Message) :> es )
    => HecoOps -> TimePhase -> Eff es Message
timePhaseToMessasge ops (TimePhase unique contents) = do
    cache <- state $ swap . LRU.lookup unique
    case cache of
        Nothing -> do
            msg <- case V.length contents of
                0 -> newUserMessage ""
                1 -> case Typeable.cast $ contents V.! 0 of
                    Just (TerminalReply _ msg) -> pure msg
                    Just (TerminalToolResponse r) -> newToolMessage r
                    _ -> createUserMsg contents
                _ -> createUserMsg contents
            modify $ LRU.insert unique msg
            pure msg
        Just msg -> pure msg
    where
        createUserMsg contents = do
            let builder = formatImmanantContents ops contents
                content = TL.toStrict $ TLB.toLazyText builder
            newUserMessage content

associateMemory ::
    ( HasCallStack
    , DatabaseService :> es
    , LanguageService :> es
    , InternalTimeStream :> es )
    => HecoOps -> Eff es (Vector AnyImmanantContent)
associateMemory ops = do
    TimePhase _ contents <- getUrimpression
    if V.length contents == 0
        then pure mempty
        else do
            let memoryOps = ops.memoryOps
                collection = memoryOps.collectionName

            loadCollection collection
            embeddings <- traverse (embedImmanantContent ops) contents
            memEnts <- searchEntities @Memory
                collection memoryOps.searchOps (V.map cast $ embeddings)

            let nubbedEnts = V.nubBy (\a b -> compare a.id b.id) memEnts
            pure $ V.map cast nubbedEnts

injectMemory ::
    ( HasCallStack
    , DatabaseService :> es
    , LanguageService :> es
    , InternalTimeStream :> es )
    => HecoOps -> Eff es ()
injectMemory ops = associateMemory ops >>= enrichUrimpression_

wrapInteraction ::
    ( HasCallStack
    , IOE :> es
    , DatabaseService :> es
    , LanguageService :> es
    , LanguageToolProvider :> es
    , InternalTimeStream :> es
    , State (LRU Unique Message) :> es
    , Reader Message :> es
    , Event EgoEvent :> es
    , Error LanguageError :> es
    , Error EgoError :> es )
    => HecoOps -> LocalEnv localEs es -> Eff localEs a -> Eff es a
wrapInteraction ops env eff = 
    startInteraction
        >> localSeqUnlift env \unlift ->
            finally (unlift eff) finalizeInteraction
    where
        startInteraction = do
            progressUrimpression_
            trigger $ OnEgoInteractionStarted

        finalizeInteraction = do
            injectMemory ops

            initialPrompt <- ask
            retention <- getRetention
            urimpression <- getUrimpression
            retentionMessages <- traverse (timePhaseToMessasge ops) retention
            urimpressionMessage <- timePhaseToMessasge ops urimpression

            let retentionCount = V.length retention
                messages = V.create do
                    messages <- VM.new $ retentionCount + 2
                    VM.write messages 0 initialPrompt
                    V.iforM_ retentionMessages \i msg -> VM.write messages (i + 1) msg
                    VM.write messages (retentionCount + 1) urimpressionMessage
                    pure messages

            trigger $ OnEgoInputMessagesGenerated messages

            tools <- getLanguageTools
            msg <- doChat (ops.chatOps { tools = ops.chatOps.tools ++ tools }) messages

            trigger $ OnEgoInteractionCompleted msg
            
        doChat chatOps messages = do
            res <- (Right <$> chat chatOps messages)
                `catchError` (\_ (e :: LanguageError) -> pure $ Left e)
            case res of
                Left e -> do
                    liftIO $ putStrLn $ displayException e
                    liftIO $ putStrLn "Error found, retrying..."
                    doChat chatOps messages
                Right msg -> do
                    progressUrimpression_
                    enrichUrimpressionSingle_ $ TerminalReply 0 msg
                    progressUrimpression_
                    handleReceivedMsg chatOps messages msg
        
        handleReceivedMsg chatOps messages = \case
            msg@(AssistantMessage _ _ []) -> pure msg

            AssistantMessage _ _ toolCalls -> do
                respMsgs <- forM toolCalls \t -> do
                    result <- invokeLanguageTool t.name t.arguments
                    let resp = ToolResponse
                            { id = t.id
                            , name = t.name
                            , content = result }
                    progressUrimpression_
                    enrichUrimpressionSingle_ $ TerminalToolResponse resp
                    newToolMessage resp

                progressUrimpression_
                doChat chatOps $ messages <> V.fromList respMsgs

            msg -> throwError $ UnhandledEgoError $
                    "invalid message from message service: " ++ show msg

runHecoEgo :: forall es a.
    ( HasCallStack
    , IOE :> es
    , DatabaseService :> es
    , LanguageService :> es
    , LanguageToolProvider :> es
    , InternalTimeStream :> es
    , Event EgoEvent :> es
    , Event InternalTimeStreamEvent :> es
    , Error EgoError :> es
    , Error LanguageError :> es )
    => HecoOps -> Eff (Ego : es) a -> Eff es a
runHecoEgo ops = reinterpret wrap \env -> \case
    InteractEgo eff -> wrapInteraction ops env eff
        `on` \case
            OnTimePhaseLost (TimePhase _ contents) ->
                memorizeImmanantContents ops contents
            _ -> pure ()
        `catch` \(e :: SomeException) ->
            (throwError . UnhandledEgoError $ displayException e)

    InjectMemory -> injectMemory ops
    where
        wrap e = do
            initialPrompt <- newSystemMessage $ ops.characterPrompt <> "\n\n" <> ops.taskPrompt
            e & evalState (LRU.newLRU @Unique @Message $ toInteger <$> ops.messageCacheLimit)
                . runReader initialPrompt

runHecoEgoEx ::
    ( HasCallStack
    , IOE :> es
    , DatabaseService :> es
    , LanguageService :> es
    , LanguageToolProvider :> es
    , InternalTimeStream :> es
    , Event InternalTimeStreamEvent :> es
    , Error LanguageError :> es )
    => HecoOps
    -> Eff (Ego : Event EgoEvent : Error EgoError : es) a
    -> Eff es (Either (CallStack, EgoError) a)
runHecoEgoEx ops = 
    runError. runEvent . runHecoEgo ops