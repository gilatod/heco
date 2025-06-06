module Heco.Effectful.InternalTimeStream.RingBuffer where

import Heco.Data.TimePhase (TimePhase(..))
import Heco.Data.TimePhase qualified as TimePhase
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
import Data.Functor ((<&>))

newtype RingBufferOps = RingBufferOps
    { capacity :: Int }

data ServiceState = ServiceState
    { present :: MVar TimePhase
    , retention :: RingBuffer Vector TimePhase }

runRingBufferInternalTimeStream ::
    ( HasCallStack
    , IOE :> es, Concurrent :> es
    , Event InternalTimeStreamEvent :> es
    , Error InternalTimeStreamError :> es )
    => RingBufferOps
    -> Eff (InternalTimeStream : es) a
    -> Eff es a
runRingBufferInternalTimeStream ops = reinterpret wrap \_ -> \case
    ProgressPresent -> do
        state <- ask @ServiceState

        let present = state.present
            retention = state.retention
            capacity = RingBuffer.capacity retention

        modifyMVar present \uri -> do
            if TimePhase.length uri == 0
                then pure (uri, uri)
                else do
                    length <- liftIO $ RingBuffer.length retention
                    lostPhase <- if length == capacity
                        then liftIO $ RingBuffer.latest retention (capacity - 1)
                        else pure Nothing

                    liftIO $ RingBuffer.append uri retention
                    whenJust lostPhase $ trigger . OnTimePhaseLost
                    trigger $ OnTimePhaseRetented uri

                    timePhase <- TimePhase.new mempty
                    pure (timePhase, uri)
        `catchIO` (throwError . UnhandledInternalTimeStreamError . displayException)

    Present newContents -> do
        state <- ask @ServiceState
        modifyMVar state.present \(TimePhase uuid contents) -> do
            let enriched = TimePhase uuid $ contents <> newContents
            trigger $ OnTimePhaseEnriched enriched newContents
            pure (enriched, enriched)

    GetPresent -> do
        state <- ask @ServiceState
        readMVar state.present

    GetRetention -> do
        state <- ask @ServiceState
        -- TODO: performance!
        liftIO (RingBuffer.toList state.retention) <&> (V.reverse . V.fromList)

    GetRetentionLength -> do
        state <- ask @ServiceState
        liftIO $ RingBuffer.length state.retention

    GetRetentionCapacity -> do
        state <- ask @ServiceState
        pure $ RingBuffer.capacity state.retention

    ClearRetention -> do
        state <- ask @ServiceState
        liftIO $ RingBuffer.clear state.retention

    where
        wrap e = do
            buffer <- liftIO $ RingBuffer.new ops.capacity
            timePhase <- TimePhase.new mempty
            present <- newMVar timePhase
            let state = ServiceState
                    { present = present
                    , retention = buffer }
            runReader state e

runRingBufferInternalTimeStreamEx ::
    (HasCallStack, IOE :> es, Concurrent :> es)
    => RingBufferOps
    -> Eff (InternalTimeStream : Event InternalTimeStreamEvent : Error InternalTimeStreamError : es) a
    -> Eff es (Either (CallStack, InternalTimeStreamError) a)
runRingBufferInternalTimeStreamEx ops =
    runError . runEvent . runRingBufferInternalTimeStream ops