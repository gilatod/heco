module Heco.Events.DatabaseEvent where

import Heco.Data.Collection (CollectionName)
import Heco.Data.Entity (Entity, EntityId)

data DatabaseEvent
    = OnDatabaseCollectionLoaded CollectionName
    | OnDatabaseCollectionReleased CollectionName
    | OnDatabaseCollectionFlushed CollectionName
    | OnDatabaseCollectionCompacted CollectionName
    | forall e. Entity e => OnDatabaseEntityUpdated e
    | OnDatabaseEntityDeleted EntityId