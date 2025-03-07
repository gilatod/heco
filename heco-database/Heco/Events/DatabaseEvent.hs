module Heco.Events.DatabaseEvent where

import Heco.Data.Collection (CollectionName)
import Heco.Data.Entity (IsEntityData, EntityId)

data DatabaseEvent
    = OnDatabaseCollectionLoaded CollectionName
    | OnDatabaseCollectionReleased CollectionName
    | OnDatabaseCollectionFlushed CollectionName
    | OnDatabaseCollectionCompacted CollectionName
    | forall e. IsEntityData e => OnDatabaseEntityUpdated e
    | OnDatabaseEntityDeleted EntityId