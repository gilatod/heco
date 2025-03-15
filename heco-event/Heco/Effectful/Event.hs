module Heco.Effectful.Event where

import Effectful (Effect, Eff, (:>))
import Effectful.Dispatch.Dynamic (reinterpret, localSeqUnlift, HasCallStack)
import Effectful.State.Static.Shared (evalState, state, get, modify, State)
import Effectful.TH (makeEffect)
import Effectful.Exception (bracket)

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
import Data.Foldable (for_)

newtype EventListenerHandle = EventListenerHandle Int
    deriving (Eq, Hashable)

data Event e :: Effect where
    Listen :: (e -> m ()) -> Event e m EventListenerHandle
    Unlisten :: EventListenerHandle -> Event e m ()
    Trigger :: e -> Event e m ()

makeEffect ''Event

listen_ :: (HasCallStack, Event e :> es)
    => (e -> Eff es ()) -> Eff es ()
listen_ f = listen f >> pure ()

on :: (HasCallStack, Event e :> es)
    => Eff es a -> (e -> Eff es ()) -> Eff es a
on e f = bracket (listen f) unlisten (const e)

data EventState e es = EventState
    (HashMap EventListenerHandle (e -> Eff (State (EventState e es) : es) ())) Int

runEvent :: forall e es a.
    HasCallStack => Eff (Event e : es) a -> Eff es a
runEvent = reinterpret (evalState emptyState) \env -> \case
    Listen listener -> localSeqUnlift env \unlift ->
        state \(EventState map acc) ->
            let handle = EventListenerHandle $ acc + 1
                map' = HashMap.insert handle (unlift . listener) map
            in (handle, EventState map' (acc + 1))
    Unlisten handle ->
        modify \(EventState map acc) ->
            let map' = HashMap.delete handle map
            in EventState map' acc
    Trigger e ->
        get >>= \(EventState map _) -> for_ map ($ e)
    where
        emptyState :: EventState e es
        emptyState = EventState HashMap.empty 0