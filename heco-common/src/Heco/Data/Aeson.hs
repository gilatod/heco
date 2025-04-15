{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Heco.Data.Aeson where

import Data.HashMap.Strict qualified as HashMap

import Data.Aeson
    ( Encoding,
      Value(Array),
      ToJSON(toJSON, toEncoding),
      Options(..),
      KeyValue(..),
      parseIndexedJSON,
      withArray )
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (FromJSON(..), Parser, listEncoding)
import Data.Aeson.Encoding (encodingToLazyByteString)

import qualified Data.Vector as V
import qualified Data.Vector.Unboxing as VU
import qualified Data.Vector.Generic as VG

import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL

defaultAesonOps :: Aeson.Options
defaultAesonOps = Aeson.defaultOptions
    { omitNothingFields = True
    , fieldLabelModifier = \case
        ('_':rs) -> rs
        name -> name }

defaultAesonOpsNotOmitNull :: Aeson.Options
defaultAesonOpsNotOmitNull = Aeson.defaultOptions
    { omitNothingFields = False
    , fieldLabelModifier = \case
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