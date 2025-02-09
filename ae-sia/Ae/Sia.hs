module Ae.Sia where

import Effectful.TH (makeEffect)
import Effectful (Effect)

import Ae.Sia.Types (
    WorldId, EntityId, RelationId, EntitySelector,
    Component (..), ComponentStorage (..), IsEntityComponentStorage)

data Sia :: Effect where
    -- world
    CreateWorld :: String -> Sia m WorldId
    LookupWorld :: String -> Sia m (Maybe WorldId)
    -- entity
    CreateEntity :: WorldId -> Sia m EntityId
    RemoveEntity :: WorldId -> EntityId -> Sia m Bool
    HasEntity :: WorldId -> EntityId -> Sia m Bool
    SetEntityName :: WorldId -> EntityId -> (Maybe String) -> Sia m ()
    GetEntityName :: WorldId -> EntityId -> Sia m (Maybe String)
    LookupEntity :: WorldId -> String -> Sia m (Maybe EntityId)
    -- singleton entity
    GetSingleton ::
        (Component c, Storage c ~ Unique)
        => WorldId -> Sia m c
    RemoveSingleton ::
        (Component c, Storage c ~ Unique)
        => WorldId -> Sia m Bool
    -- entity component
    RegisterComponent ::
        (Component c) => WorldId -> Sia m ()
    AddComponent ::
        (Component c, IsEntityComponentStorage (Storage c) ~ True)
        => WorldId -> EntityId -> c -> Sia m ()
    RemoveComponent ::
        (Component c, IsEntityComponentStorage (Storage c) ~ True)
        => WorldId -> EntityId -> Sia m Bool
    GetComponent ::
        (Component c, IsEntityComponentStorage (Storage c) ~ True)
        => WorldId -> EntityId -> Sia m c
    HasComponent ::
        (Component c, IsEntityComponentStorage (Storage c) ~ True)
        => WorldId -> EntityId -> Sia m Bool
    -- global component
    AddGlobalComponent ::
        (Component c, Storage c ~ Global)
        => WorldId -> c -> Sia m ()
    RemoveGlobalComponent ::
        (Component c, Storage c ~ Global)
        => WorldId -> Sia m Bool
    GetGlobalComponent ::
        (Component c, Storage c ~ Global)
        => WorldId -> Sia m c
    -- relationship
    AddRelation ::
        (Component c, IsEntityComponentStorage (Storage c) ~ True)
        => WorldId -> EntityId -> c -> EntityId -> Sia m RelationId
    RemoveRelationById
        :: WorldId -> RelationId -> Sia m Bool
    RemoveRelation ::
        (Component c, IsEntityComponentStorage (Storage c) ~ True)
        => WorldId -> EntityId -> EntityId -> Sia m Bool
    HasRelation ::
        (Component c, IsEntityComponentStorage (Storage c) ~ True)
        => WorldId -> EntitySelector -> EntitySelector -> Sia m Bool
    GetRelation ::
        (Component c, IsEntityComponentStorage (Storage c) ~ True)
        => WorldId -> RelationId -> Sia m (c, EntityId, EntityId)
    GetRelationPair :: WorldId -> RelationId -> Sia m (EntityId, EntityId)
    
makeEffect ''Sia