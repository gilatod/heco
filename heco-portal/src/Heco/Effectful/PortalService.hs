module Heco.Effectful.PortalService where

import Heco.Data.Portal (Portal(..), PortalSignal(..))
import Heco.Data.Immanant.Terminal (TerminalId(..))
import Heco.Events.EgoEvent (EgoEvent(..))
import Heco.Effectful.Event (Event, withEvent)

import Effectful (Effect, Eff, (:>), UnliftStrategy(ConcUnlift), Persistence(Persistent), Limit(Unlimited))
import Effectful.TH (makeEffect)
import Effectful.Dispatch.Dynamic (reinterpret, localLiftUnlift)
import Effectful.State.Static.Shared (evalState, gets, state, modify, State)
import Effectful.Concurrent (Concurrent, forkIO, killThread)
import Effectful.Concurrent.Chan (newChan, readChan, writeChan, Chan)

import Conduit (repeatMC)

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Text (Text)

import Control.Concurrent (ThreadId)

data PortalService :: Effect where
    RunPortal :: Portal m -> PortalService m TerminalId
    HasPortal :: TerminalId -> PortalService m Bool
    GetPortalIds :: PortalService m [TerminalId]
    GetPortalName :: TerminalId -> PortalService m (Maybe Text)
    KillPortal :: TerminalId -> PortalService m Bool
    SendToPortal :: TerminalId -> PortalSignal -> PortalService m Bool

makeEffect ''PortalService

sendToPortal_ :: PortalService :> es => TerminalId -> PortalSignal -> Eff es ()
sendToPortal_ id sig = sendToPortal id sig >> pure ()

closePortal :: PortalService :> es => TerminalId -> Eff es Bool
closePortal id = sendToPortal id PortalClose

closePortal_ :: PortalService :> es => TerminalId -> Eff es ()
closePortal_ id = closePortal id >> pure ()

data PortalState = PortalState
    { name :: Text
    , threadId :: ThreadId
    , signalChan :: Chan PortalSignal }

type PortalStates = HashMap TerminalId PortalState

lookupPortalState :: State PortalStates :> es => TerminalId -> Eff es (Maybe PortalState)
lookupPortalState id = gets @PortalStates $ HashMap.lookup id

doSendToPortal ::
    ( Concurrent :> es
    , State PortalStates :> es )
    => TerminalId -> PortalSignal -> Eff es Bool
doSendToPortal id sig = do
    res <- lookupPortalState id
    case res of
        Nothing -> pure False
        Just ps -> do
            writeChan ps.signalChan sig
            pure True

runStandardPortalService :: forall es a.
    (Concurrent :> es, Event EgoEvent :> es)
    => Eff (PortalService : es) a -> Eff es a
runStandardPortalService = reinterpret wrap \env -> \case
    RunPortal portal -> do
        id <- state \i -> let i' = i + 1 in (i', i')
        chan <- newChan
        localLiftUnlift env (ConcUnlift Persistent Unlimited) \lift unlift -> do
            let conduit = repeatMC $ lift $ readChan chan
            tid <- forkIO $ unlift $ portal.procedure id conduit
            modify $ HashMap.insert id PortalState
                { name = portal.name
                , threadId = tid
                , signalChan = chan }
            pure id
    HasPortal id -> gets @PortalStates $ HashMap.member id
    GetPortalIds -> gets @PortalStates HashMap.keys
    GetPortalName id -> lookupPortalState id >>= pure . fmap (\ps -> ps.name)
    KillPortal id -> do
        res <- lookupPortalState id
        case res of
            Nothing -> pure False
            Just ps -> do
                killThread ps.threadId
                pure True
    SendToPortal id sig -> doSendToPortal id sig
    where
        wrap =
            evalState (HashMap.empty :: PortalStates)
            . evalState (0 :: TerminalId)
            . withEvent \case
                OnEgoReply phase id content ->
                    doSendToPortal id (PortalReply phase content) >> pure ()
                _ -> pure ()