module Heco.Effectful.PortalService where

import Heco.Data.Portal (Portal(..), PortalSignal(..))
import Heco.Data.Immanant.Terminal (SessionId(..))
import Heco.Events.AgentEvent (AgentEvent(..))
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
    RunPortal :: Portal m -> PortalService m SessionId
    HasPortal :: SessionId -> PortalService m Bool
    ListPortalIds :: PortalService m [SessionId]
    GetPortalName :: SessionId -> PortalService m (Maybe Text)
    KillPortal :: SessionId -> PortalService m Bool
    SendToPortal :: SessionId -> PortalSignal -> PortalService m Bool

makeEffect ''PortalService

runPortal_ :: PortalService :> es => Portal (Eff es) -> Eff es ()
runPortal_ portal = void $ runPortal portal

killPortal_ :: PortalService :> es => SessionId -> Eff es ()
killPortal_ id = void $ killPortal_ id

sendToPortal_ :: PortalService :> es => SessionId -> PortalSignal -> Eff es ()
sendToPortal_ id sig = void $ sendToPortal id sig

closePortal :: PortalService :> es => SessionId -> Eff es Bool
closePortal id = sendToPortal id PortalClose

closePortal_ :: PortalService :> es => SessionId -> Eff es ()
closePortal_ id = void $ closePortal id

data PortalState = PortalState
    { name :: Text
    , threadId :: ThreadId
    , signalChan :: Chan PortalSignal }

type PortalMap = HashMap SessionId PortalState

lookupPortal :: State PortalMap :> es => SessionId -> Eff es (Maybe PortalState)
lookupPortal id = gets @PortalMap $ HashMap.lookup id

removePortal :: State PortalMap :> es => SessionId -> Eff es (Maybe PortalState)
removePortal id = state @PortalMap $ HashMap.alterF (, Nothing) id

doSendToPortal ::
    ( Concurrent :> es
    , State PortalMap :> es )
    => SessionId -> PortalSignal -> Eff es Bool
doSendToPortal id sig = do
    res <- lookupPortal id
    case res of
        Nothing -> pure False
        Just ps -> do
            writeChan ps.signalChan sig
            pure True

runStandardPortalService :: forall es a.
    ( HasCallStack
    , Concurrent :> es, Event AgentEvent :> es )
    => Eff (PortalService : es) a -> Eff es a
runStandardPortalService = reinterpret wrap \env -> \case
    RunPortal (Portal name procedure) -> do
        id <- state $ dupe . (+1)
        chan <- newChan
        localLiftUnlift env unliftStrategy \lift unlift -> do
            let conduit = repeatMC $ lift $ readChan chan
            tid <- forkIO $
                unlift (procedure id conduit)
                    `finally` removePortal id
            modify $ HashMap.insert id PortalState
                { name = name
                , threadId = tid
                , signalChan = chan }
            pure id
    HasPortal id -> gets @PortalMap $ HashMap.member id
    ListPortalIds -> gets @PortalMap HashMap.keys
    GetPortalName id -> lookupPortal id <&> fmap \ps -> ps.name
    KillPortal id -> do
        res <- removePortal id
        case res of
            Nothing -> pure False
            Just ps -> do
                killThread ps.threadId
                pure True
    SendToPortal id sig -> doSendToPortal id sig
    where
        wrap =
            evalState (HashMap.empty :: PortalMap)
            . evalState (0 :: SessionId)
            . withEvent \case
                OnAgentReply phase id content ->
                    void $ doSendToPortal id $ PortalReply phase content
                _ -> pure ()
        unliftStrategy = ConcUnlift Persistent Unlimited