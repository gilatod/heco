module Heco.Data.Entity where

import Data.Aeson
    ( Zero,
      FromJSON,
      Value(Object),
      GToJSON',
      ToJSON (toJSON),
      genericToJSON,
      defaultOptions )
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM

import Data.Text (Text)
import Data.Vector.Unboxing qualified as VU
import Data.Default (Default (def))
import Data.Hashable (Hashable)

import Pattern.Cast (Cast(..))
import GHC.Records (HasField)
import Heco.Data.Aeson (HasAesonOps(aesonOpsNotOmitNull))
import GHC.Generics (Generic (..))

newtype EntityId = EntityId Int
    deriving (Show, Eq, Ord, Enum, Bounded)
    deriving newtype Hashable

instance Default EntityId where
    def = EntityId 0

instance VU.Unboxable EntityId where
    type Rep EntityId = Int

deriveJSON defaultOptions ''EntityId

instance Cast EntityId Int where
    cast (EntityId t) = t

class
    ( Default e, Generic e
    , HasAesonOps e
    , GToJSON' Value Zero (Rep e)
    , ToJSON e, FromJSON e
    , HasField "id" e (Maybe EntityId) )
    => Entity e where
    entityNonDataFields :: [Text]
    entityNonDataFields = ["vector", "sparse_vector"]

data SomeEntity = forall e. Entity e => SomeEntity e

instance Show SomeEntity where
    show (SomeEntity e) = "Entity " ++ show (toJSON e)

entityDataFields :: forall e. Entity e => [Text]
entityDataFields =
    case genericToJSON (aesonOpsNotOmitNull @e) (def :: e) of
        Object obj -> filter filterFunc $ map K.toText (KM.keys obj)
        _ -> []
    where
        filterFunc t = t `notElem` (entityNonDataFields @e)