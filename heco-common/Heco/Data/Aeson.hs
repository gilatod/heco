{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Heco.Data.Aeson where

import Data.HashMap.Strict qualified as HashMap

import Data.Aeson
    ( Encoding,
      Value(Array),
      ToJSON(toJSON, toEncoding),
      Options(fieldLabelModifier),
      parseIndexedJSON,
      withArray )
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (FromJSON(..), Parser, listEncoding)

import qualified Data.Vector as V
import qualified Data.Vector.Unboxing as VU
import qualified Data.Vector.Generic as VG

defaultAesonOps :: Aeson.Options
defaultAesonOps = Aeson.defaultOptions
    { fieldLabelModifier = \case
        ('_':rs) -> rs
        name -> name }

aesonOps :: [(String, String)] -> Aeson.Options
aesonOps pairs = Aeson.defaultOptions
    { fieldLabelModifier = \case
        ('_':rs) -> rs
        name -> case HashMap.lookup name map of
            Just key -> key
            Nothing -> name }
    where
        map = HashMap.fromList pairs

vectorParseJSON :: (FromJSON a, VG.Vector w a) => String -> Value -> Parser (w a)
vectorParseJSON s = withArray s $ fmap V.convert . V.mapM (uncurry $ parseIndexedJSON parseJSON) . V.indexed

instance (VG.Vector VU.Vector a, FromJSON a) => FromJSON (VU.Vector a) where
    parseJSON = vectorParseJSON "Data.Vector.Unboxed.Vector"

{-# INLINE encodeVector #-}
encodeVector :: (ToJSON a, VG.Vector v a) => v a -> Encoding
encodeVector = listEncoding toEncoding . VG.toList

{-# INLINE vectorToJSON #-}
vectorToJSON :: (VG.Vector v a, ToJSON a) => v a -> Value
vectorToJSON = Array . V.map toJSON . V.convert

instance (VG.Vector VU.Vector a, ToJSON a) => ToJSON (VU.Vector a) where
    toJSON = vectorToJSON
    toEncoding = encodeVector