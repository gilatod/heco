{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE UndecidableInstances #-}

module Heco.Data.Aeson where

import Data.Aeson
    ( genericParseJSON,
      parseIndexedJSON,
      withArray,
      genericToEncoding,
      genericToJSON,
      Encoding,
      FromJSON(parseJSON),
      GFromJSON,
      Zero,
      Options(..),
      Value(Array),
      GToJSON',
      KeyValue(..),
      ToJSON(toJSON, toEncoding), SumEncoding (..) )
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Parser, listEncoding)
import Data.Aeson.Encoding (encodingToLazyByteString)

import Data.Vector qualified as V
import Data.Vector.Unboxing qualified as VU
import Data.Vector.Generic qualified as VG

import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL

import GHC.Generics (Generic (Rep))

class HasAesonOps a where
    aesonOps :: Aeson.Options
    aesonOps = defaultAesonOps

    aesonOpsNotOmitNull :: Aeson.Options
    aesonOpsNotOmitNull = (aesonOps @a) { omitNothingFields = False }

newtype AesonDefault t = AesonDefault t
    deriving HasAesonOps via t

instance
    ( Generic a, HasAesonOps a
    , GToJSON' Encoding Zero (Rep a)
    , GToJSON' Value Zero (Rep a) )
    => ToJSON (AesonDefault a) where
    toJSON (AesonDefault a) = genericToJSON (aesonOps @a) a
    toEncoding (AesonDefault a) = genericToEncoding (aesonOps @a) a

instance
    ( Generic a, HasAesonOps a
    , GFromJSON Zero (Rep a) )
    => FromJSON (AesonDefault a) where
    parseJSON v = AesonDefault <$> genericParseJSON (aesonOps @a) v

defaultAesonOps :: Aeson.Options
defaultAesonOps = Aeson.defaultOptions
    { omitNothingFields = True
    , sumEncoding = UntaggedValue
    , fieldLabelModifier = \case
        ('_':rs) -> rs
        name -> name }

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

(.=.) :: forall e kv. KeyValue e kv => Aeson.Key -> e -> kv
k .=. v = explicitToField id k v

(.=?) ::
    (KeyValue e kv, ToJSON v, Monoid kv)
    => Aeson.Key -> Maybe v -> kv
(.=?) name = \case
    Just a -> name .= a
    Nothing -> mempty

(.=.?) ::
    (KeyValue e kv, Monoid kv)
    => Aeson.Key -> Maybe e -> kv
(.=.?) name = \case
    Just a -> name .=. a
    Nothing -> mempty

infixl 8 .=., .=?, .=.?

encodingToLazyText :: Encoding -> TL.Text
encodingToLazyText = TL.decodeUtf8 . encodingToLazyByteString

encodingToText :: Encoding -> T.Text
encodingToText = TL.toStrict . encodingToLazyText