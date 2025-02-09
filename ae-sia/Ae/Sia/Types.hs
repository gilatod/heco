{-# LANGUAGE UndecidableInstances #-}

module Ae.Sia.Types where

import GHC.Generics (Generic)
import Data.Data (Proxy (Proxy))
import GHC.Records (HasField (..))
import GHC.TypeLits (symbolVal, KnownSymbol)

-- ids

newtype WorldId = WorldId Int
    deriving (Show, Eq)

newtype EntityId = EntityId Int
    deriving (Show, Eq)

newtype RelationId = RelationId Int
    deriving (Show, Eq)

-- component

data ComponentStorage = Instanced | Unique | Global
    deriving (Enum)

type family IsEntityComponentStorage (c :: ComponentStorage) :: Bool
type instance IsEntityComponentStorage Instanced = True
type instance IsEntityComponentStorage Unique = True
type instance IsEntityComponentStorage Global = False

class ComponentStorageToValue (s :: ComponentStorage) where
    componentStorageToValue :: ComponentStorage

instance ComponentStorageToValue Instanced where componentStorageToValue = Instanced
instance ComponentStorageToValue Unique where componentStorageToValue = Unique
instance ComponentStorageToValue Global where componentStorageToValue = Global

class (Generic c) => Component c where
    type Storage c :: ComponentStorage

componentStorage ::
    forall c. (ComponentStorageToValue (Storage c))
    => ComponentStorage
componentStorage = componentStorageToValue @(Storage c)

componentStorageFromProxy ::
    forall c. (ComponentStorageToValue (Storage c))
    => Proxy c -> ComponentStorage
componentStorageFromProxy _ = componentStorage @c

componentStorageFromValue ::
    forall c. (ComponentStorageToValue (Storage c))
    => c -> ComponentStorage
componentStorageFromValue _ = componentStorage @c

-- field

data FieldAccessor r = FieldAccessor r
    deriving (Eq, Show)
data FieldAccess r t = FieldAccess r String
    deriving (Eq, Show)

instance (KnownSymbol k, HasField k r t)
    => HasField k (FieldAccessor r) (FieldAccess r t) where
    getField (FieldAccessor r) = FieldAccess r (symbolVal (Proxy :: Proxy k))

instance (KnownSymbol k, HasField k r2 t)
    => HasField k (FieldAccess r1 r2) (FieldAccess (FieldAccess r1 r2) t) where
    getField a = FieldAccess a (symbolVal (Proxy :: Proxy k))

-- selectors

data EntitySelector = EntitySelector EntityId | EntityWildcard

data EntityQuery
    = forall c. (Component c, IsEntityComponentStorage (Storage c) ~ True)
        => With
    | forall c. (Component c, IsEntityComponentStorage (Storage c) ~ True)
        => Without
    | forall c. (Component c, IsEntityComponentStorage (Storage c) ~ True)
        => WithRelation EntitySelector
    | WithAnyRelation EntitySelector
    | forall c. (Component c, IsEntityComponentStorage (Storage c) ~ True)
        => WithoutRelation EntitySelector
    | WithoutAnyRelation EntitySelector
    | AllOf [EntityQuery]
    | AnyOf [EntityQuery]

data RelationQuery
    = forall c. (Component c, IsEntityComponentStorage (Storage c) ~ True)
        => RelationQuery EntitySelector
    | AnyRelationQuery EntitySelector

-- query

newtype QueryPlaceholder t = QueryPlaceholder Int

class QuerySelectable s
instance QuerySelectable WorldId
instance QuerySelectable EntityId
instance QuerySelectable RelationId