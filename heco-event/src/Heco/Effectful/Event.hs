module Heco.Effectful.Event where

import Effectful
    ( raise,
      type (:>),
      Effect,
      Eff,
      Limit(Unlimited),
      Persistence(Persistent),
      UnliftStrategy(ConcUnlift) )
import Effectful.Dispatch.Dynamic (reinterpret, HasCallStack, localUnlift)
import Effectful.State.Static.Shared (evalState, state, get, modify, State, runState)
import Effectful.TH (makeEffect)
import Effectful.Exception (bracket)

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
import Data.Foldable (for_)

import Control.Monad (void)

newtype EventListenerHandle = EventListenerHandle Int
    deriving Eq
    deriving newtype Hashable

data Event e :: Effect where
    Listen :: (e -> m ()) -> Event e m EventListenerHandle
    Unlisten :: EventListenerHandle -> Event e m ()
    Trigger :: e -> Event e m ()

makeEffect ''Event

listen_ :: (HasCallStack, Event e :> es)
    => (e -> Eff es ()) -> Eff es ()
listen_ f = void $ listen f

on :: forall e es a.
    (HasCallStack, Event e :> es)
    => Eff es a -> (e -> Eff es ()) -> Eff es a
on e f = bracket (listen f) (unlisten @e) (const e)

collect :: forall e es a b.
    (HasCallStack, Event e :> es)
    => Eff es a -> (e -> Eff es b) -> Eff es (a, [b])
collect m f = runState [] $
    bracket (listen doCollect) (unlisten @e) (const $ raise m)
    where
        doCollect e = raise (f e) >>= modify . (:)

withEvent :: forall e es a.
    (HasCallStack, Event e :> es)
    => (e -> Eff es ()) -> Eff es a -> Eff es a
withEvent = flip on

data EventState e es = EventState
    (HashMap EventListenerHandle (e -> Eff (State (EventState e es) : es) ())) Int

runEvent :: forall e es a.
    HasCallStack => Eff (Event e : es) a -> Eff es a
runEvent = reinterpret (evalState emptyState) \env -> \case
    Listen listener -> localUnlift env unliftStrategy \unlift ->
        state \(EventState map acc) ->
            let handle = EventListenerHandle $ acc + 1
                map' = HashMap.insert handle (unlift . listener) map
            in (handle, EventState map' (acc + 1))
    Unlisten handle ->
        modify @(EventState e es) \(EventState map acc) ->
            let map' = HashMap.delete handle map
            in EventState map' acc
    Trigger e ->
        get >>= \(EventState map _) -> for_ map ($ e)
    where
        emptyState :: EventState e es
        emptyState = EventState HashMap.empty 0
        unliftStrategy = ConcUnlift Persistent Unlimited