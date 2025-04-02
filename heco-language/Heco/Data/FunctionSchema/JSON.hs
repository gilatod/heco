{-# OPTIONS_GHC -Wno-orphans #-}

module Heco.Data.FunctionSchema.JSON where

import Heco.Data.FunctionSchema
    ( PropertySchema(name, optional, description, schema),
      ObjectSpec(additionalProperties, properties),
      ArrayItemSpec(TupleItems, ArrayItems, UniqueItems),
      ArraySpec(maximumLength, items, additionalItems, minimumLength),
      NumberSpec(exclusiveMaximum, minimum, maximum, exclusiveMinimum),
      IntegerSpec(exclusiveMaximum, minimum, maximum, exclusiveMinimum),
      EnumOption(description, value),
      StringFormat(..),
      StringSpec(format, pattern, minLength, maxLength),
      DataSchema(..),
      FunctionSchema(description, parameters, name) )
import Heco.Data.Aeson ((.=?), (.=.), (.=.?))

import Data.Aeson (ToJSON(..), Encoding, (.=), pairs, KeyValue(..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.Encoding (list)

import Data.Text (Text)
import GHC.Records (HasField)

instance ToJSON FunctionSchema where
    -- toJSON = fromJust . decode @Value . encodingToLazyByteString . encodeFunctionSchema
    toJSON = undefined
    toEncoding = encodeFunctionSchema

encodeFunctionSchema :: FunctionSchema -> Encoding
encodeFunctionSchema s = pairs $
    "type" .= ("function" :: Text) <>
    "function" .=. function
    where
        function = pairs $
            "name" .= s.name <>
            "description" .=? s.description <>
            "parameters" .=. params
        params = pairs $ "type" .= ("object" :: Text) <> encodeObjectSpecKV s.parameters

encodeDataSchemaKV :: (KeyValue Encoding kv, Monoid kv) => DataSchema -> kv
encodeDataSchemaKV = \case
    StringSchema s -> doEncode "string" $ encodeStringSpecKV s
    StringEnumSchema ss -> doEncode "string" $ encodeEnumSpecsKV ss
    IntegerSchema s -> doEncode "integer" $ encodeNumberSpecKV s
    IntegerEnumSchema ss -> doEncode "integer" $ encodeEnumSpecsKV ss
    NumberSchema s -> doEncode "number" $ encodeNumberSpecKV s
    NumberEnumSchema ss -> doEncode "number" $ encodeEnumSpecsKV ss
    BoolSchema -> doEncode "boolean" mempty
    ArraySchema s -> doEncode "array" $ encodeArraySpecKV s
    ObjectSchema s -> doEncode "object" $ encodeObjectSpecKV s
    NullSchema -> doEncode "null" mempty
    where
        doEncode (t :: Text) rest = "type" .= t <> rest

encodeDataSchema :: DataSchema -> Encoding
encodeDataSchema = pairs . encodeDataSchemaKV

encodeStringSpecKV :: (KeyValue Encoding kv, Monoid kv) => StringSpec -> kv
encodeStringSpecKV s = mconcat
    [ "minLength" .=? s.minLength
    , "maxLength" .=? s.maxLength
    , "pattern" .=? s.pattern
    , "format" .=? (encodeStringFormat <$> s.format) ]

encodeStringFormat :: StringFormat -> Text
encodeStringFormat = \case
    DateTimeFormat -> "date-time"
    TimeFormat -> "time"
    DateFormat -> "date"
    DurationFormat -> "duration"
    EmailFormat -> "email"
    IPv4Format -> "ipv4"
    IPv6Format -> "ipv6"
    UUIDFormat -> "uuid"
    URIFormat -> "uri"
    PointerFormat -> "json-pointer"
    RegexFormat -> "regex"

encodeEnumSpecsKV :: (KeyValue Encoding kv, ToJSON v) => [EnumOption v] -> kv
encodeEnumSpecsKV =
    ("enum" .=.) . list \s ->
        pairs $ "value" .= s.value
                <> "description" .=? s.description
    
encodeNumberSpecKV ::
    ( KeyValue Encoding kv, Monoid kv
    , ToJSON n
    , HasField "minimum"s (Maybe n)
    , HasField "maximum" s (Maybe n)
    , HasField "exclusiveMinimum" s Bool
    , HasField "exclusiveMaximum" s Bool  )
    => s -> kv
encodeNumberSpecKV s = mconcat
    [ "minimum" .=? s.minimum
    , "maximum" .=? s.maximum
    , "exclusiveMinimum" .= s.exclusiveMinimum
    , "exclusiveMaximum"  .= s.exclusiveMaximum ]

encodeArraySpecKV :: (KeyValue Encoding kv, Monoid kv) => ArraySpec -> kv
encodeArraySpecKV s = mconcat
    [ "items" .=. encodeArrayItemSpec s.items
    , "additionalItems" .=.? (encodeDataSchema <$> s.additionalItems)
    , "minimumLength" .=? s.minimumLength
    , "maximumLength" .=? s.maximumLength ]

encodeArrayItemSpec :: ArrayItemSpec -> Encoding
encodeArrayItemSpec = \case
    ArrayItems s -> encodeDataSchema s
    UniqueItems s -> encodeDataSchema s
    TupleItems ss -> list encodeDataSchema ss

encodeObjectSpecKV :: (KeyValue Encoding kv, Monoid kv) => ObjectSpec -> kv
encodeObjectSpecKV s = mconcat
    [ "properties" .=. (pairs . mconcat . map encodePropertySchemaKV) s.properties
    , "additionalProperties" .=.? (encodeDataSchema <$> s.additionalProperties)
    , "required" .= (map (\s -> s.name) . filter (\s -> not s.optional)) s.properties ]
 
encodePropertySchemaKV :: KeyValue Encoding kv => PropertySchema -> kv
encodePropertySchemaKV s = Key.fromText s.name .=. pairs inner
    where
        inner = "description" .=? s.description <> encodeDataSchemaKV s.schema

encodePropertySchema :: PropertySchema -> Encoding
encodePropertySchema = pairs . encodePropertySchemaKV