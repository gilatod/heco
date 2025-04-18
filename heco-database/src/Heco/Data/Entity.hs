{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Heco.Data.Entity where

import Data.Aeson
    ( Zero,
      FromJSON,
      Value(Object),
      GToJSON',
      ToJSON,
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
    deriving (Show, Eq, Hashable, Ord, Enum, Bounded)

instance VU.Unboxable EntityId where
    type Rep EntityId = Int

deriveJSON defaultOptions ''EntityId

instance Cast EntityId Int where
    cast (EntityId t) = t

type IsEntityData e =
    ( ToJSON e
    , HasField "id" e (Maybe EntityId)
    , HasField "vector" e (Maybe (VU.Vector Float)) )

class
    ( Default e, Generic e
    , HasAesonOps e
    , GToJSON' Value Zero (Rep e), FromJSON e
    , IsEntityData e)
    => Entity e where
    entityDataFields :: [Text]
    entityDataFields = case genericToJSON (aesonOpsNotOmitNull @e) (def :: e) of
        Object obj ->
            filter (\t -> t /= "id" && t /= "vector") $ map K.toText (KM.keys obj)
        _ -> []