module Heco.Effectful.InternalTimeStream.RingBuffer where

import Heco.Data.TimePhase (TimePhase(..), emptyTimePhase)
import Heco.Data.InternalTimeStreamError (InternalTimeStreamError(..))
import Heco.Events.InternalTimeStreamEvent (InternalTimeStreamEvent(..))
import Heco.Effectful.Event (Event, trigger, runEvent)
import Heco.Effectful.InternalTimeStream (InternalTimeStream(..))

import Effectful (Eff, (:>), IOE, MonadIO (liftIO))
import Effectful.Dispatch.Dynamic (HasCallStack, reinterpret)
import Effectful.Exception (catchIO, Exception(displayException))
import Effectful.Reader.Static (runReader, ask)
import Effectful.Concurrent.MVar (MVar, readMVar, newMVar, Concurrent, modifyMVar)
import Effectful.Error.Dynamic (throwError, Error, CallStack, runError)

import Data.RingBuffer (RingBuffer)
import Data.RingBuffer qualified as RingBuffer
import Data.Vector (Vector)
import Data.Vector qualified as V
import Control.Monad.Extra (whenJust)

data RingBufferOps = RingBufferOps
    { capacity :: Int }

data ServiceState = ServiceState
    { urimpression :: MVar TimePhase
    , retention :: RingBuffer Vector TimePhase }

runRingBufferInternalTimeStream ::
    ( HasCallStack
    , IOE :> es, Concurrent :> es
    , Event InternalTimeStreamEvent :> es
    , Error InternalTimeStreamError :> es )
    => RingBufferOps
    -> Eff (InternalTimeStream : es) a
    -> Eff es a
runRingBufferInternalTimeStream ops = reinterpret evalServiceState \_ -> \case
    ProgressUrimpression -> do
        state <- ask @ServiceState

        let urimpression = state.urimpression
            retention = state.retention
            capacity = RingBuffer.capacity retention

        modifyMVar urimpression \uri@(TimePhase contents) -> do
            if V.length contents == 0
                then pure (uri, uri)
                else do
                    length <- liftIO $ RingBuffer.length retention
                    lostPhase <- if length == capacity
                        then liftIO $ RingBuffer.latest retention (capacity - 1)
                        else pure Nothing

                    liftIO $ RingBuffer.append uri retention
                    whenJust lostPhase $ trigger . OnTimePhaseLost
                    trigger $ OnTimePhaseRetented uri

                    pure (emptyTimePhase, uri)
        `catchIO` (\e -> throwError . UnhandledInternalTimeStreamError $ displayException e)

    EnrichUrimpression newContents -> do
        state <- ask @ServiceState
        modifyMVar state.urimpression \(TimePhase contents) -> do
            let enriched = TimePhase $ newContents <> contents
            trigger $ OnTimePhaseEnriched enriched newContents
            pure (enriched, enriched)

    GetUrimpression -> do
        state <- ask @ServiceState
        readMVar state.urimpression

    GetRetention -> do
        state <- ask @ServiceState
        -- TODO: performance!
        liftIO (RingBuffer.toList state.retention)
            >>= pure . V.reverse . V.fromList

    GetRetentionLength -> do
        state <- ask @ServiceState
        liftIO $ RingBuffer.length state.retention

    GetRetentionCapacity -> do
        state <- ask @ServiceState
        pure $ RingBuffer.capacity state.retention

    where
        evalServiceState e = do
            buffer <- liftIO $ RingBuffer.new ops.capacity
            urimpression <- newMVar emptyTimePhase
            let state = ServiceState
                    { urimpression = urimpression
                    , retention = buffer }
            runReader state e

runRingBufferInternalTimeStreamEx ::
    (HasCallStack, IOE :> es, Concurrent :> es)
    => RingBufferOps
    -> Eff (InternalTimeStream : Event InternalTimeStreamEvent : Error InternalTimeStreamError : es) a
    -> Eff es (Either (CallStack, InternalTimeStreamError) a)
runRingBufferInternalTimeStreamEx ops = 
    runError . runEvent . runRingBufferInternalTimeStream ops