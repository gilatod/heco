{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}

module Heco.Effectful.Agent.Heco where

import Heco.Data.Default ()
import Heco.Data.Model (ModelName)
import Heco.Data.Embedding (Embedding)
import Heco.Data.TimePhase
    ( TimePhase(..),
      ImmanantContent(..),
      AnyImmanantContent(AnyImmanantContent),
      joinImmanantContent, castImmanantContent )
import Heco.Data.Immanant.Memory (Memory(..))
import Heco.Data.Immanant.Terminal (Terminal(..))
import Heco.Data.Collection (CollectionName)
import Heco.Data.Message
    ( messageText,
      newSystemMessage,
      newToolMessage,
      newUserMessage,
      Message(ToolMessage, UserMessage, AssistantMessage),
      ToolCall(id, name, arguments),
      ToolResponse(content, ToolResponse, id, name) )
import Heco.Data.LanguageError (LanguageError)
import Heco.Data.AgentError (AgentError(..))
import Heco.Data.TimePhase qualified as TimePhase
import Heco.Events.AgentEvent (AgentEvent(..))
import Heco.Events.InternalTimeStreamEvent (InternalTimeStreamEvent(..))
import Heco.Effectful.Event (Event, trigger, runEvent, collect)
import Heco.Effectful.DatabaseService
    ( SearchOps(..),
      DatabaseService,
      searchEntities, SearchData (DenseVectorData), addEntity_ )
import Heco.Effectful.LanguageService
    ( chat, embed, ChatOps(tools), LanguageService )
import Heco.Effectful.LanguageToolProvider
    ( LanguageToolProvider,
      getLanguageToolSchemas,
      invokeLanguageTool ) 
import Heco.Effectful.InternalTimeStream
    ( InternalTimeStream,
      present_,
      getRetention,
      getPresent,
      progressPresent_, presentOne_ )
import Heco.Effectful.Agent (Agent(..))

import Effectful (Eff, (:>), MonadIO (liftIO), IOE)
import Effectful.Dispatch.Dynamic (localSeqUnlift, HasCallStack, reinterpret)
import Effectful.State.Static.Shared (State, modify, state, evalState)
import Effectful.Reader.Static (runReader, Reader, ask)
import Effectful.Error.Dynamic (Error, throwError, runError, CallStack, catchError)
import Effectful.Exception (finally, Exception(displayException))
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.QSem (QSem, waitQSem, signalQSem, newQSem)

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
import Data.Time (getCurrentTime)
import Data.Function ((&))
import Data.Cache.LRU (LRU)
import Data.Cache.LRU qualified as LRU
import Data.Unique (Unique)
import Data.Tuple (swap)
import Data.String (IsString(fromString))
import Data.Aeson.QQ (aesonQQ)
import Data.Char (isSpace)

import Control.Applicative ((<|>))
import Control.Monad (forM, unless, (>=>))
import Control.Monad.Extra (whenJustM)
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
    , memorizingPrompt :: Text
    , chatOps :: ChatOps
    , memoryOps :: HecoMemoryOps
    , immanantContentFormatter :: AnyImmanantContent -> TLB.Builder
    , messageCacheLimit :: Maybe Int }

data HecoInternal = HecoInternal
    { initialMessage :: Message
    , memorizingPrompt :: Text }

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
            msg <- if V.length contents == 0
                then newUserMessage ""
                else case castImmanantContent $ contents V.! 0 of
                    Just (TerminalReply _ msg) -> pure msg
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
    TimePhase _ contents <- getPresent
    if V.length contents == 0
        then pure mempty
        else do
            let memoryOps = ops.memoryOps
                collection = memoryOps.collectionName
            embeddings <- contents & traverse (embedImmanantContent ops >=> (pure . DenseVectorData))
            memEnts <- searchEntities @Memory collection memoryOps.searchOps embeddings
            pure $ V.map cast $ V.nubBy (\a b -> compare a.id b.id) memEnts

injectMemory ::
    ( HasCallStack
    , DatabaseService :> es
    , LanguageService :> es
    , InternalTimeStream :> es )
    => HecoOps -> Eff es ()
injectMemory ops = associateMemory ops >>= present_

wrapInteraction ::
    ( HasCallStack
    , IOE :> es
    , Concurrent :> es
    , DatabaseService :> es
    , LanguageService :> es
    , LanguageToolProvider :> es
    , InternalTimeStream :> es
    , State (LRU Unique Message) :> es
    , Reader HecoInternal :> es
    , Reader QSem :> es
    , Event AgentEvent :> es
    , Event InternalTimeStreamEvent :> es
    , Error LanguageError :> es
    , Error AgentError :> es )
    => HecoOps -> Maybe TimePhase -> Eff es a -> Eff es a
wrapInteraction ops replyingTimePhase eff = do
    startInteraction
    flip finally finalizeInteraction do
        (res, lostPhases) <- eff `collect` \case
            OnTimePhaseLost timePhase -> pure $ Just timePhase
            _ -> pure Nothing
        respond lostPhases
        pure res
    where
        startInteraction = do
            ask @QSem >>= waitQSem
            progressPresent_
            trigger OnAgentInteractionStarted

        respond lostPhases1 = do
            injectMemory ops

            internal <- ask @HecoInternal
            retention <- getRetention
            present <- getPresent
            retentionMessages <- traverse (timePhaseToMessasge ops) retention
            presentMessage <- timePhaseToMessasge ops present

            let retentionCount = V.length retention
                messages = V.create do
                    let fstElem = if retentionCount /= 0
                            then Just $ retentionMessages V.! 0
                            else Nothing
                    case fstElem of
                        Just (ToolMessage _ _) -> do
                            messages <- VM.new $ retentionCount + 1
                            V.iforM_ retentionMessages $ VM.write messages
                            VM.write messages 0 internal.initialMessage
                            VM.write messages retentionCount presentMessage
                            pure messages
                        _ -> do
                            messages <- VM.new $ retentionCount + 2
                            VM.write messages 0 internal.initialMessage
                            V.iforM_ retentionMessages \i msg -> VM.write messages (i + 1) msg
                            VM.write messages (retentionCount + 1) presentMessage
                            pure messages

            trigger $ OnAgentInputMessagesGenerated messages

            schemas <- getLanguageToolSchemas
            let chatOps = ops.chatOps { tools = schemas ++ ops.chatOps.tools }

            ((msg, prevMsgs), lostPhases2) <-
                runReader (replyingTimePhase <|> Just present) $
                    finally (doChat chatOps messages) progressPresent_
                    `collect` \case
                        OnTimePhaseLost timePhase -> pure $ Just timePhase
                        _ -> pure Nothing

            let lostPhases = lostPhases1 ++ lostPhases2
            unless (null lostPhases) do
                runReader (Nothing :: Maybe TimePhase) do
                    memorizeLostPhases
                        chatOps internal.memorizingPrompt (prevMsgs <> V.singleton msg) lostPhases

            trigger $ OnAgentInteractionCompleted msg

        finalizeInteraction = do
            ask @QSem >>= signalQSem

        memorizeLostPhases _ _ _ [] = pure ()
        memorizeLostPhases chatOps prompt msgs (Nothing:restPhases) =
            memorizeLostPhases chatOps prompt msgs restPhases
        memorizeLostPhases chatOps prompt msgs (Just timePhase:restPhases) = do
            let doMemorize = memorizeText chatOps prompt msgs
                memorizeRest = memorizeLostPhases chatOps prompt msgs restPhases
            phaseMsg <- timePhaseToMessasge ops timePhase
            case phaseMsg of
                UserMessage _ text ->
                    doMemorize text >> memorizeRest
                AssistantMessage _ statement reasoning _ ->
                    doMemorize statement >> doMemorize reasoning >> memorizeRest
                _ -> memorizeRest

        memorizeText chatOps prompt prevMsgs content = do
            promptMsg <- newUserMessage $ prompt <> "\n" <> content
            (msg, _) <- doChat chatOps $ prevMsgs <> V.singleton promptMsg

            let summary = messageText msg
            embedding <- embed ops.memoryOps.embeddingModel summary
            time <- liftIO getCurrentTime

            addEntity_ ops.memoryOps.collectionName Memory
                { id = Nothing
                , vector = Just embedding
                , content = summary
                , metadata = Just [aesonQQ|{ create_time: #{time} }|] }

        doChat chatOps messages = do
            res <- (Right <$> chat chatOps messages)
                `catchError` (\_ (e :: LanguageError) -> pure $ Left e)
            case res of
                Left e -> do
                    liftIO $ putStrLn $ displayException e
                    liftIO $ putStrLn "Error found, retrying..."
                    doChat chatOps messages
                Right msg -> do
                    whenJustM (ask @(Maybe TimePhase)) \phase -> do
                        progressPresent_
                        presentOne_ $ TerminalReply phase msg
                        progressPresent_
                    handleReceivedMsg chatOps messages msg

        handleReceivedMsg chatOps messages = \case
            msg@(AssistantMessage _ statement _ []) -> do
                trySendReply statement
                pure (msg, messages)

            msg@(AssistantMessage _ statement _ toolCalls) -> do
                unless (T.all isSpace statement) do
                    trySendReply statement
                respMsgs <- forM toolCalls \t -> do
                    result <- invokeLanguageTool t.name t.arguments
                    let resp = ToolResponse
                            { id = t.id
                            , name = t.name
                            , content = either (fromString . displayException) id result }
                    trigger $ OnAgentToolUsed t resp
                    toolMsg <- newToolMessage resp
                    whenJustM (ask @(Maybe TimePhase)) \phase -> do
                        presentOne_ $ TerminalReply phase toolMsg
                        progressPresent_
                    pure toolMsg
                doChat chatOps $ messages <> V.fromList (msg:respMsgs)

            msg -> throwError $ AgentInvalidReplyError $
                "invalid message from message service: " ++ show msg

        trySendReply content =
            whenJustM ask \present ->
                case TimePhase.getImmanantContent @Terminal present of
                    Just (TerminalChat id _) -> trigger $ OnAgentReply present id content
                    Just (TerminalTaskResponse id _ _) -> trigger $ OnAgentReply present id content
                    _ -> pure ()

runHecoAgent :: forall es a.
    ( HasCallStack
    , IOE :> es
    , Concurrent :> es
    , DatabaseService :> es
    , LanguageService :> es
    , LanguageToolProvider :> es
    , InternalTimeStream :> es
    , Event AgentEvent :> es
    , Event InternalTimeStreamEvent :> es
    , Error AgentError :> es
    , Error LanguageError :> es )
    => HecoOps -> Eff (Agent : es) a -> Eff es a
runHecoAgent ops = reinterpret wrap \env -> \case
    WithAgentInteractionEx replyingPhase eff ->
        localSeqUnlift env \unlift ->
            wrapInteraction ops replyingPhase (unlift eff)
    where
        wrap e = do
            initialPrompt <- newSystemMessage $ ops.characterPrompt <> "\n\n" <> ops.taskPrompt
            qsem <- newQSem 1
            e & evalState (LRU.newLRU @Unique @Message $ toInteger <$> ops.messageCacheLimit)
                . runReader qsem
                . runReader HecoInternal
                    { initialMessage = initialPrompt
                    , memorizingPrompt = ops.memorizingPrompt }

runHecoAgentEx ::
    ( HasCallStack
    , IOE :> es
    , Concurrent :> es
    , DatabaseService :> es
    , LanguageService :> es
    , LanguageToolProvider :> es
    , InternalTimeStream :> es
    , Event InternalTimeStreamEvent :> es
    , Error LanguageError :> es )
    => HecoOps
    -> Eff (Agent : Event AgentEvent : Error AgentError : es) a
    -> Eff es (Either (CallStack, AgentError) a)
runHecoAgentEx ops =
    runError. runEvent . runHecoAgent ops