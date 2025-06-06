module Heco.Effectful.DatabaseService where

import Heco.Data.Aeson (defaultAesonOps)
import Heco.Data.Entity (Entity, EntityId)
import Heco.Data.Collection (CollectionName)

import Effectful (Effect, (:>), Eff)
import Effectful.TH (makeEffect)

import Data.Aeson.TH (deriveJSON)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector.Unboxing qualified as VU
import Data.Default (Default(..))

import GHC.Generics (Generic)
import Control.Monad (void)

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

data SearchData
    = DenseVectorData (VU.Vector Float)
    | SparseVectorData
        { indices :: VU.Vector Float
        , values :: VU.Vector Float }
    | TextData Text

deriveJSON defaultAesonOps ''SearchData

data SearchOps = SearchOps
    { vectorField :: Text
    , filter :: Maybe Text
    , limit :: Maybe Int
    , offset :: Maybe Int
    , radius :: Maybe Float
    , rangeFilter :: Maybe Float }
    deriving (Generic, Show, Eq)

instance Default SearchOps where
    def = SearchOps
        { vectorField = "vector"
        , filter = Nothing
        , limit = Nothing
        , offset = Nothing
        , radius = Nothing
        , rangeFilter = Nothing }

data DatabaseService :: Effect where
    LoadCollection :: CollectionName -> DatabaseService m ()
    RefreshLoadCollection :: CollectionName -> DatabaseService m ()
    ReleaseCollection :: CollectionName -> DatabaseService m ()
    FlushCollection :: CollectionName -> DatabaseService m ()
    CompactCollection :: CollectionName -> DatabaseService m ()
    GetCollectionLoadState :: CollectionName -> DatabaseService m CollectionLoadState
    GetEntityCount :: CollectionName -> DatabaseService m Int

    CreateEntity :: Entity e => CollectionName -> DatabaseService m EntityId
    AddEntities :: Entity e => CollectionName -> Vector e -> DatabaseService m (VU.Vector EntityId)
    AddEntity :: Entity e => CollectionName -> e -> DatabaseService m EntityId
    SetEntities :: Entity e => CollectionName -> Vector e -> DatabaseService m (VU.Vector EntityId)
    SetEntity :: Entity e => CollectionName -> e -> DatabaseService m EntityId
    GetEntities :: Entity e => CollectionName -> VU.Vector EntityId -> DatabaseService m (Vector e)
    GetEntity :: Entity e => CollectionName -> EntityId -> DatabaseService m (Maybe e)
    QueryEntities :: Entity e => CollectionName -> QueryOps -> DatabaseService m (Vector e)
    SearchEntities :: Entity e => CollectionName -> SearchOps -> Vector SearchData -> DatabaseService m (Vector e)
    DeleteEntities :: CollectionName -> Text -> DatabaseService m ()

makeEffect ''DatabaseService

addEntity_ :: (DatabaseService :> es, Entity e)
    => CollectionName -> e -> Eff es ()
addEntity_ c e = void $ addEntity c e

addEntities_ :: (DatabaseService :> es, Entity e)
    => CollectionName -> Vector e -> Eff es ()
addEntities_ c es = void $ addEntities c es

setEntity_ :: (DatabaseService :> es, Entity e)
    => CollectionName -> e -> Eff es ()
setEntity_ c e = void $ setEntity c e

setEntities_ :: (DatabaseService :> es, Entity e)
    => CollectionName -> Vector e -> Eff es ()
setEntities_ c es = void $ setEntities c es