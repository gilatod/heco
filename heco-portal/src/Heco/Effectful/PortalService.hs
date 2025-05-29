module Heco.Effectful.PortalService where

import Heco.Data.Portal (Portal(..), PortalSignal(..))
import Heco.Data.Immanant.Terminal (TerminalId(..))
import Heco.Events.EgoEvent (EgoEvent(..))
import Heco.Effectful.Event (Event, withEvent)

import Effectful (Effect, Eff, (:>), UnliftStrategy(ConcUnlift), Persistence(Persistent), Limit(Unlimited))
import Effectful.TH (makeEffect)
import Effectful.Dispatch.Dynamic (HasCallStack, reinterpret, localLiftUnlift)
import Effectful.State.Static.Shared (evalState, gets, state, modify, State)
import Effectful.Concurrent (Concurrent, forkIO, killThread)
import Effectful.Concurrent.Chan (newChan, readChan, writeChan, Chan)
import Effectful.Exception (finally)

import Conduit (repeatMC)

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Text (Text)
import Data.Tuple.Extra (dupe)
import Data.Functor ((<&>))

import Control.Concurrent (ThreadId)
import Control.Monad (void)

data PortalService :: Effect where
    RunPortal :: Portal m -> PortalService m TerminalId
    HasPortal :: TerminalId -> PortalService m Bool
    GetPortalIds :: PortalService m [TerminalId]
    GetPortalName :: TerminalId -> PortalService m (Maybe Text)
    KillPortal :: TerminalId -> PortalService m Bool
    SendToPortal :: TerminalId -> PortalSignal -> PortalService m Bool

makeEffect ''PortalService

runPortal_ :: (HasCallStack, PortalService :> es) => Portal (Eff es) -> Eff es ()
runPortal_ portal = void $ runPortal portal

killPortal_ :: (HasCallStack, PortalService :> es) => TerminalId -> Eff es ()
killPortal_ id = void $ killPortal_ id

sendToPortal_ :: (HasCallStack, PortalService :> es) => TerminalId -> PortalSignal -> Eff es ()
sendToPortal_ id sig = void $ sendToPortal id sig

closePortal :: (HasCallStack, PortalService :> es) => TerminalId -> Eff es Bool
closePortal id = sendToPortal id PortalClose

closePortal_ :: (HasCallStack, PortalService :> es) => TerminalId -> Eff es ()
closePortal_ id = void $ closePortal id

data PortalState = PortalState
    { name :: Text
    , threadId :: ThreadId
    , signalChan :: Chan PortalSignal }

type PortalStates = HashMap TerminalId PortalState

lookupPortalState :: State PortalStates :> es => TerminalId -> Eff es (Maybe PortalState)
lookupPortalState id = gets @PortalStates $ HashMap.lookup id

removePortalState :: State PortalStates :> es => TerminalId -> Eff es (Maybe PortalState)
removePortalState id = state @PortalStates $ HashMap.alterF (, Nothing) id

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
    ( HasCallStack
    , Concurrent :> es, Event EgoEvent :> es )
    => Eff (PortalService : es) a -> Eff es a
runStandardPortalService = reinterpret wrap \env -> \case
    RunPortal (Portal name procedure) -> do
        id <- state $ dupe . (+1)
        chan <- newChan
        localLiftUnlift env (ConcUnlift Persistent Unlimited) \lift unlift -> do
            let conduit = repeatMC $ lift $ readChan chan
            tid <- forkIO $
                unlift (procedure id conduit)
                    `finally` removePortalState id
            modify $ HashMap.insert id PortalState
                { name = name
                , threadId = tid
                , signalChan = chan }
            pure id
    HasPortal id -> gets @PortalStates $ HashMap.member id
    GetPortalIds -> gets @PortalStates HashMap.keys
    GetPortalName id -> lookupPortalState id <&> fmap \ps -> ps.name
    KillPortal id -> do
        res <- removePortalState id
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
                    void $ doSendToPortal id (PortalReply phase content)
                _ -> pure ()