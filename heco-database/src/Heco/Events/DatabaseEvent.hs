module Heco.Events.DatabaseEvent where

import Heco.Data.Collection (CollectionName)
import Heco.Data.Entity (SomeEntity, EntityId)

data DatabaseEvent
    = OnDatabaseCollectionLoaded CollectionName
    | OnDatabaseCollectionReleased CollectionName
    | OnDatabaseCollectionFlushed CollectionName
    | OnDatabaseCollectionCompacted CollectionName
    | OnDatabaseEntityUpdated SomeEntity
    | OnDatabaseEntityDeleted EntityId
    deriving (Show)