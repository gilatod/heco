module Ae.Sia.SiaQuery.Entity where

import Effectful (Effect)

import Ae.Sia.Types
    ( QueryPlaceholder,
      Component(..),
      IsEntityComponentStorage,
      ComponentStorage(Unique),
      EntityId )

data SiaEntityQuery :: Effect where
    Singleton ::
        (Component c, Storage c ~ Unique)
        => SiaEntityQuery m (QueryPlaceholder c)
    With ::
        (Component c, IsEntityComponentStorage (Storage c) ~ True)
        => QueryPlaceholder EntityId
        -> SiaEntityQuery m ()
    Without ::
        (Component c, IsEntityComponentStorage (Storage c) ~ True)
        => QueryPlaceholder EntityId
        -> SiaEntityQuery m ()
    Access ::
        (Component c, IsEntityComponentStorage (Storage c) ~ True)
        => QueryPlaceholder EntityId
        -> (c -> a)
        -> SiaEntityQuery m a 