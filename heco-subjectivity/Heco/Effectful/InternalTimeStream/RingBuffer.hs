module Heco.Effectful.InternalTimeStream.RingBuffer where

import Heco.Data.TimePhase (TimePhase(..), emptyTimePhase)
import Heco.Events.InternalTimeStreamEvent (InternalTimeStreamEvent(..))
import Heco.Effectful.Event (Event, trigger)
import Heco.Effectful.InternalTimeStream (InternalTimeStream(..))

import Effectful (Eff, (:>), IOE, MonadIO (liftIO))
import Effectful.Dispatch.Dynamic (HasCallStack, reinterpret)
import Effectful.Reader.Static (runReader, ask)
import Effectful.Concurrent.MVar (MVar, takeMVar, newMVar, modifyMVar_, Concurrent)

import Data.RingBuffer (RingBuffer)
import Data.RingBuffer qualified as RingBuffer
import Data.Vector (Vector)
import Control.Monad.Extra (whenJust)

data RingBufferOps = RingBufferOps
    { capacity :: Int }

data ServiceState = ServiceState
    { urimpression :: MVar TimePhase
    , retention :: RingBuffer Vector TimePhase }

runRingBufferInternalTimeStream ::
    ( HasCallStack
    , IOE :> es, Concurrent :> es
    , Event InternalTimeStreamEvent :> es )
    => RingBufferOps
    -> Eff (InternalTimeStream : es) a
    -> Eff es a
runRingBufferInternalTimeStream ops = reinterpret evalServiceState \_ -> \case
    ProgressUrimpression -> do
        state <- ask
        modifyMVar_ state.urimpression \uri -> do
            let retention = state.retention
                capacity = RingBuffer.capacity retention

            length <- liftIO $ RingBuffer.length retention
            lostPhase <- if length == capacity
                then liftIO $ RingBuffer.latest retention (capacity - 1)
                else pure Nothing

            liftIO $ RingBuffer.append uri retention
            whenJust lostPhase $ trigger . TimePhaseLostEvent
            trigger $ TimePhaseRetentedEvent uri

            pure emptyTimePhase
    
    EnrichUrimpression content -> do
        state <- ask
        modifyMVar_ state.urimpression \uri -> do
            let enriched = uri { contents = content : uri.contents }
            trigger $ TimePhaseEnrichedEvent enriched content
            pure enriched
    
    Urimpression -> do
        state <- ask
        takeMVar state.urimpression
    
    Retention -> do
        state <- ask
        liftIO $ RingBuffer.toList state.retention
    
    GetRetentionLength -> do
        state <- ask
        liftIO $ RingBuffer.length state.retention
    
    GetRetentionCapacity -> do
        state <- ask
        pure $ RingBuffer.capacity state.retention

    where
        evalServiceState e = do
            buffer <- liftIO $ RingBuffer.new ops.capacity
            urimpression <- newMVar emptyTimePhase
            let state = ServiceState
                    { urimpression = urimpression
                    , retention = buffer }
            runReader state e