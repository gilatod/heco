module Heco.Effectful.ImmanentTime.RingBuffer where

import Heco.Data.MonoHFunctor (MonoHFunctor(ohmap))
import Heco.Data.TimePhase (TimePhase(..))
import Heco.Data.TimePhase qualified as TimePhase
import Heco.Data.Immanent (Immanent(noesis))
import Heco.Data.ImmanentTimeError (ImmanentTimeError(..))
import Heco.Events.ImmanentTimeEvent (ImmanentTimeEvent(..))
import Heco.Effectful.Exception (throwUnhandled)
import Heco.Effectful.Event (Event, trigger, runEvent)
import Heco.Effectful.ImmanentTime (ImmanentTime(..))

import Effectful (Eff, (:>), IOE, MonadIO (liftIO), raise)
import Effectful.Dispatch.Dynamic (HasCallStack, reinterpret, localSeqUnlift, localSeqLift)
import Effectful.Exception (catchIO, Exception(displayException))
import Effectful.Reader.Static (runReader, ask)
import Effectful.Concurrent.MVar (MVar, readMVar, newMVar, Concurrent, modifyMVar)
import Effectful.Error.Dynamic (throwError, Error, runErrorWith)

import Data.RingBuffer (RingBuffer)
import Data.RingBuffer qualified as RingBuffer
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Functor ((<&>))

import Control.Monad.Extra (whenJust)

newtype RingBufferOps = RingBufferOps
    { capacity :: Int }

data ServiceState es = ServiceState
    { present :: MVar (TimePhase (Eff es))
    , retention :: RingBuffer Vector (TimePhase (Eff es)) }

runRingBufferImmanentTime :: forall es a.
    ( HasCallStack
    , IOE :> es, Concurrent :> es
    , Event ImmanentTimeEvent :> es
    , Error ImmanentTimeError :> es )
    => RingBufferOps
    -> Eff (ImmanentTime : es) a
    -> Eff es a
runRingBufferImmanentTime ops = reinterpret wrap \env -> \case
    ProgressPresent -> do
        state <- ask @(ServiceState es)

        let present = state.present
            retention = state.retention
            capacity = RingBuffer.capacity retention
        
        TimePhase _ immanents <- modifyMVar present \p -> do
            if TimePhase.length p == 0
                then pure (p, p)
                else do
                    length <- liftIO $ RingBuffer.length retention
                    lostPhase <- if length == capacity
                        then liftIO $ RingBuffer.latest retention (capacity - 1)
                        else pure Nothing

                    liftIO $ RingBuffer.append p retention
                    whenJust lostPhase $ trigger . OnTimePhaseLost

                    timePhase <- TimePhase.new mempty
                    trigger $ OnTimePhaseRetented p

                    pure (ohmap (runReader state) timePhase, p)
            `catchIO` (throwError . UnhandledImmanentTimeError . displayException)
        
        immanents' <- V.mapMaybeM (raise . noesis) immanents
        modifyMVar present \p -> do
            let present' = TimePhase.insertImmanents immanents' p
            trigger $ OnTimePhaseEnriched present' immanents'
            pure (present', ())

    Present immanents -> do
        state <- ask @(ServiceState es)
        modifyMVar state.present \(TimePhase uuid prevImmanents) -> do
            localSeqUnlift env \unlift -> do
                let immanents' = V.map (ohmap $ runReader state . unlift) immanents
                    enriched = TimePhase uuid $ prevImmanents <> immanents'
                trigger $ OnTimePhaseEnriched enriched immanents'
                pure (enriched, ())

    GetPresent -> do
        state <- ask @(ServiceState es)
        localSeqLiftTimePhase env \lift ->
            lift <$> readMVar state.present

    GetRetention -> do
        state <- ask @(ServiceState es)
        -- TODO: performance!
        localSeqLiftTimePhase env \lift ->
            liftIO (RingBuffer.toList state.retention) <&> (V.reverse . V.fromList . map lift)

    GetRetentionLength -> do
        state <- ask @(ServiceState es)
        liftIO $ RingBuffer.length state.retention

    GetRetentionCapacity -> do
        state <- ask @(ServiceState es)
        pure $ RingBuffer.capacity state.retention

    ClearRetention -> do
        state <- ask @(ServiceState es)
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
        
        localSeqLiftTimePhase env f =
            localSeqLift env \lift -> f $ ohmap (lift . raise)

runRingBufferImmanentTimeEx ::
    (HasCallStack, IOE :> es, Concurrent :> es)
    => RingBufferOps
    -> Eff (ImmanentTime : Event ImmanentTimeEvent : Error ImmanentTimeError : es) a
    -> Eff es a
runRingBufferImmanentTimeEx ops =
    runErrorWith throwUnhandled . runEvent . runRingBufferImmanentTime ops