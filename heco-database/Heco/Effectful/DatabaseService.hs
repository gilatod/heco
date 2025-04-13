module Heco.Effectful.DatabaseService where

import Heco.Data.Entity (IsEntityData, Entity, EntityId)
import Heco.Data.Collection (CollectionName)

import Effectful (Effect, (:>), Eff)
import Effectful.TH (makeEffect)
import Effectful.Dispatch.Dynamic (HasCallStack)

import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector.Unboxing qualified as VU
import Data.Default (Default(..))
import GHC.Generics (Generic)

data CollectionLoadState = CollectionLoadState
    { progress :: Int
    , state :: Text
    , message :: Text }
    deriving (Show, Eq)

data QueryOps = QueryOps
    { filter :: Text
    , limit :: Maybe Int
    , offset :: Maybe Int }
    deriving (Show, Eq)

queryOps :: Text -> QueryOps
queryOps filter = QueryOps
    { filter = filter
    , limit = Nothing
    , offset = Nothing }

data SearchOps = SearchOps
    { filter :: Maybe Text
    , limit :: Maybe Int
    , offset :: Maybe Int
    , radius :: Maybe Float
    , rangeFilter :: Maybe Float }
    deriving (Generic, Show, Eq)

instance Default SearchOps

data DatabaseService :: Effect where
    LoadCollection :: CollectionName -> DatabaseService m ()
    RefreshLoadCollection :: CollectionName -> DatabaseService m ()
    ReleaseCollection :: CollectionName -> DatabaseService m ()
    FlushCollection :: CollectionName -> DatabaseService m ()
    CompactCollection :: CollectionName -> DatabaseService m ()
    GetCollectionLoadState :: CollectionName -> DatabaseService m CollectionLoadState
    GetEntityCount :: CollectionName -> DatabaseService m Int

    CreateEntity :: (IsEntityData e, Default e) => CollectionName -> DatabaseService m EntityId
    AddEntities :: IsEntityData e => CollectionName -> Vector e -> DatabaseService m (VU.Vector EntityId)
    AddEntity :: IsEntityData e => CollectionName -> e -> DatabaseService m EntityId
    SetEntities :: IsEntityData e => CollectionName -> Vector e -> DatabaseService m (VU.Vector EntityId)
    SetEntity :: IsEntityData e => CollectionName -> e -> DatabaseService m EntityId
    GetEntities :: Entity e => CollectionName -> VU.Vector EntityId -> DatabaseService m (Vector e)
    GetEntity :: Entity e => CollectionName -> EntityId -> DatabaseService m e
    QueryEntities :: Entity e => CollectionName -> QueryOps -> DatabaseService m (Vector e)
    SearchEntities :: Entity e => CollectionName -> SearchOps -> Vector (VU.Vector Float) -> DatabaseService m (Vector e)
    DeleteEntities :: CollectionName -> Text -> DatabaseService m ()

makeEffect ''DatabaseService

addEntity_ :: (HasCallStack, DatabaseService :> es, IsEntityData e)
    => CollectionName -> e -> Eff es ()
addEntity_ c e = addEntity c e >> pure ()

addEntities_ :: (HasCallStack, DatabaseService :> es, IsEntityData e)
    => CollectionName -> Vector e -> Eff es ()
addEntities_ c es = addEntities c es >> pure ()

setEntity_ :: (HasCallStack, DatabaseService :> es, IsEntityData e)
    => CollectionName -> e -> Eff es ()
setEntity_ c e = setEntity c e >> pure ()

setEntities_ :: (HasCallStack, DatabaseService :> es, IsEntityData e)
    => CollectionName -> Vector e -> Eff es ()
setEntities_ c es = setEntities c es >> pure ()