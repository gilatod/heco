module Ae.Sia.SiaWorld where

import Effectful.TH ( makeEffect )
import Effectful ( Effect, Eff, (:>) )
import Effectful.Dispatch.Dynamic ( interpret )

import qualified Ae.Sia as Sia
import Ae.Sia.Types
    ( Component(..),
      IsEntityComponentStorage,
      ComponentStorage(Global, Unique),
      EntityId,
      WorldId )

data SiaWorld :: Effect where
    -- entity
    CreateEntity :: SiaWorld m EntityId
    RemoveEntity :: EntityId -> SiaWorld m Bool
    HasEntity :: EntityId -> SiaWorld m Bool
    SetEntityName :: EntityId -> (Maybe String) -> SiaWorld m ()
    GetEntityName :: EntityId -> SiaWorld m (Maybe String)
    LookupEntity :: String -> SiaWorld m (Maybe EntityId)
    -- singleton entity
    GetSingleton ::
        (Component c, Storage c ~ Unique)
        => SiaWorld m c
    RemoveSingleton ::
        (Component c, Storage c ~ Unique)
        => SiaWorld m Bool
    -- entity component
    AddComponent ::
        (Component c, IsEntityComponentStorage (Storage c) ~ True)
        => EntityId -> c -> SiaWorld m ()
    RemoveComponent ::
        (Component c, IsEntityComponentStorage (Storage c) ~ True)
        => EntityId -> SiaWorld m Bool
    GetComponent ::
        (Component c, IsEntityComponentStorage (Storage c) ~ True)
        => EntityId -> SiaWorld m c
    -- global component
    AddGlobalComponent ::
        (Component c, Storage c ~ Global)
        => c -> SiaWorld m ()
    RemoveGlobalComponent ::
        (Component c, Storage c ~ Global)
        => SiaWorld m Bool
    GetGlobalComponent ::
        (Component c, Storage c ~ Global)
        => SiaWorld m c

makeEffect ''SiaWorld

runWorld ::
    (Sia.Sia :> es)
    => WorldId
    -> Eff (SiaWorld : es) a
    -> Eff es a
runWorld worldId = interpret $ \_ -> \case
    -- entity
    CreateEntity -> Sia.createEntity worldId
    RemoveEntity entityId -> Sia.removeEntity worldId entityId
    HasEntity entityId -> Sia.hasEntity worldId entityId
    SetEntityName entityId name -> Sia.setEntityName worldId entityId name
    GetEntityName entityId -> Sia.getEntityName worldId entityId
    LookupEntity name -> Sia.lookupEntity worldId name
    -- singleton entity
    GetSingleton @c -> Sia.getSingleton @c worldId
    RemoveSingleton @c -> Sia.removeSingleton @c worldId
    -- entity component
    AddComponent entityId c -> Sia.addComponent worldId entityId c
    RemoveComponent @c entityId -> Sia.removeComponent @c worldId entityId
    GetComponent @c entityId -> Sia.getComponent @c worldId entityId
    -- global component
    AddGlobalComponent c -> Sia.addGlobalComponent worldId c
    RemoveGlobalComponent @c -> Sia.removeGlobalComponent @c worldId
    GetGlobalComponent @c -> Sia.getGlobalComponent @c worldId