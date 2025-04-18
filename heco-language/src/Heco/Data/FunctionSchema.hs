{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Heco.Data.FunctionSchema where

import Heco.Data.Aeson ((.=?), (.=.), (.=.?), HasAesonOps(aesonOps))
import Heco.Data.Typelits (KnownSymbols (symbolValues))
import Heco.Data.Record (RecordFieldsEx(..), FieldInfoEx(..), recordFieldsEx)

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL

import Data.Vector qualified as V
import Data.Vector.Primitive qualified as VP
import Data.Vector.Unboxing qualified as VU
import Data.Vector.Storable qualified as VS

import Data.Aeson (Key, FromJSON, ToJSON(..), Encoding, (.=), pairs, KeyValue(..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.Encoding (list)

import Data.Default (Default (..))
import Data.Time (UTCTime, LocalTime, DiffTime, TimeOfDay)
import Data.UUID (UUID)
import Data.Monoid (Dual, First, Last, Product, Sum, All, Any)
import Data.Semigroup qualified as Semigroup
import Data.Functor.Identity (Identity)
import Data.Ord (Down)
import Data.IntSet (IntSet)
import Data.List.NonEmpty (NonEmpty)
import Data.HashSet (HashSet)
import Data.HashMap.Strict qualified as HashMap
import Data.Map.Strict qualified as Map

import Data.String (IsString)
import Data.Function ((&))
import Data.Proxy (Proxy(..))
import Data.Typeable (typeRepTyCon, tyConName)

import Network.URI (URI)

import Numeric.Natural (Natural)
import Pattern.Cast (Cast(..))
import GHC.Generics (Generic, Rep)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import GHC.Records (HasField(..))

class ParametricSpec p s where
    spec :: p -> s

data FunctionSchema = FunctionSchema
    { name :: Text
    , description :: Maybe Text
    , parameters :: ObjectSpec }

data DataSchema
    = StringSchema StringSpec
    | StringEnumSchema [EnumOption Text]
    | IntegerSchema IntegerSpec
    | IntegerEnumSchema [EnumOption Int]
    | NumberSchema NumberSpec
    | NumberEnumSchema [EnumOption Float]
    | BoolSchema
    | ArraySchema ArraySpec
    | ObjectSchema ObjectSpec
    | NullSchema

data StringSpec = StringSpec
    { minLength :: Maybe Int
    , maxLength :: Maybe Int
    , pattern :: Maybe Text
    , format :: Maybe StringFormat }
    deriving (Generic, Default)

instance ParametricSpec StringFormat StringSpec where
    spec format = StringSpec
        { minLength = Nothing
        , maxLength = Nothing
        , pattern = Nothing
        , format = Just format }

data StringFormat
    = DateTimeFormat
    | TimeFormat
    | DateFormat
    | DurationFormat
    | EmailFormat
    | IPv4Format
    | IPv6Format
    | UUIDFormat
    | URIFormat
    | PointerFormat
    | RegexFormat

data EnumOption t = EnumOption
    { value :: t
    , description :: Maybe Text }

opt :: v -> EnumOption v
opt v = EnumOption v Nothing

optDesc :: v -> Text -> EnumOption v
optDesc v desc = EnumOption v (Just desc)

data IntegerSpec = IntegerSpec
    { minimum :: Maybe Int
    , maximum :: Maybe Int
    , exclusiveMinimum :: Bool
    , exclusiveMaximum :: Bool }
    deriving (Generic, Default)

instance ParametricSpec (Maybe Int, Maybe Int) IntegerSpec where
    spec (min, max) = IntegerSpec
        { minimum = min
        , maximum = max
        , exclusiveMinimum = False
        , exclusiveMaximum = False }

data NumberSpec = NumberSpec
    { minimum :: Maybe Float
    , maximum :: Maybe Float
    , exclusiveMinimum :: Bool
    , exclusiveMaximum :: Bool }
    deriving (Generic, Default)

instance ParametricSpec (Maybe Float, Maybe Float) NumberSpec where
    spec (min, max) = NumberSpec
        { minimum = min
        , maximum = max
        , exclusiveMinimum = False
        , exclusiveMaximum = False }

data ArraySpec = ArraySpec
    { items :: ArrayItemSpec
    , additionalItems :: Maybe DataSchema
    , minimumLength :: Maybe Int
    , maximumLength :: Maybe Int }

data ArrayItemSpec
    = ArrayItems DataSchema
    | UniqueItems DataSchema
    | TupleItems [DataSchema]

instance Default ArraySpec where
    def = ArraySpec
        { items = ArrayItems NullSchema
        , additionalItems = Nothing
        , minimumLength = Nothing
        , maximumLength = Nothing }

instance ParametricSpec ArrayItemSpec ArraySpec where
    spec itemSpec = ArraySpec
        { items = itemSpec
        , additionalItems = Nothing
        , minimumLength = Nothing
        , maximumLength = Nothing }

data ObjectSpec = ObjectSpec
    { properties :: [PropertySchema]
    , additionalProperties :: Maybe DataSchema }

data PropertySchema = PropertySchema
    { name :: Text
    , description :: Maybe Text
    , schema :: DataSchema
    , optional :: Bool }

instance Default ObjectSpec where
    def = ObjectSpec
        { properties = []
        , additionalProperties = Nothing }

instance ParametricSpec [PropertySchema] ObjectSpec where
    spec props = ObjectSpec
        { properties = props
        , additionalProperties = Nothing }

instance ToJSON FunctionSchema where
    -- toJSON = fromJust . decode @Value . encodingToLazyByteString . encodeFunctionSchema
    toJSON = undefined
    toEncoding s = pairs $
        "type" .= ("function" :: Text) <>
        "function" .=. function
        where
            function = pairs $
                "name" .= s.name <>
                "description" .=? s.description <>
                "parameters" .=. params
            params = pairs $ "type" .= ("object" :: Text) <> encodeObjectSpecKV s.parameters

instance ToJSON DataSchema where
    toJSON = undefined
    toEncoding = pairs . encodeDataSchemaKV

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

encodeEnumSpecsKV :: (KeyValue Encoding kv, Monoid kv, ToJSON v) => [EnumOption v] -> kv
encodeEnumSpecsKV opts = enums opts <> enumDescs opts
    where
        enums = ("enum" .=.) . list \s -> toEncoding s.value
        enumDescs = ("enumDescriptions" .=.) . list \s ->
            maybe "" toEncoding s.description
    
encodeNumberSpecKV ::
    ( KeyValue Encoding kv, Monoid kv
    , ToJSON n
    , HasField "minimum" s (Maybe n)
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
    , "additionalItems" .=.? (toEncoding <$> s.additionalItems)
    , "minimumLength" .=? s.minimumLength
    , "maximumLength" .=? s.maximumLength
    , tryEncodeUniqueness s.items ]
    where
        encodeArrayItemSpec = \case
            ArrayItems s -> toEncoding s
            UniqueItems s -> toEncoding s
            TupleItems ss -> list toEncoding ss
        tryEncodeUniqueness = \case
            UniqueItems _ -> "uniqueItems" .= True
            _ -> mempty

encodeObjectSpecKV :: (KeyValue Encoding kv, Monoid kv) => ObjectSpec -> kv
encodeObjectSpecKV s = mconcat
    [ "properties" .=. (pairs . mconcat . map encodePropertySchemaKV) s.properties
    , "additionalProperties" .=.? (toEncoding <$> s.additionalProperties)
    , "required" .= (map (\s -> s.name) . filter (\s -> not s.optional)) s.properties ]
 
encodePropertySchemaKV :: KeyValue Encoding kv => PropertySchema -> kv
encodePropertySchemaKV s = Key.fromText s.name .=. pairs inner
    where
        inner = "description" .=? s.description <> encodeDataSchemaKV s.schema

encodePropertySchema :: PropertySchema -> Encoding
encodePropertySchema = pairs . encodePropertySchemaKV

property :: Text -> DataSchema -> PropertySchema
property name schema = PropertySchema
    { name = name
    , description = Nothing
    , schema = schema
    , optional = False }

optional :: PropertySchema -> PropertySchema
optional s = s { optional = True }

describe :: Text -> PropertySchema -> PropertySchema
describe t s = s { description = Just t }

propString :: Text -> StringSpec -> PropertySchema
propString name spec = property name $ StringSchema spec

propStringEnum :: Text -> [EnumOption Text] -> PropertySchema
propStringEnum name opts = property name $ StringEnumSchema opts

propInteger :: Text -> IntegerSpec -> PropertySchema
propInteger name spec = property name $ IntegerSchema spec

propIntegerEnum :: Text -> [EnumOption Int] -> PropertySchema
propIntegerEnum name opts = property name $ IntegerEnumSchema opts

propNumber :: Text -> NumberSpec -> PropertySchema
propNumber name spec = property name $ NumberSchema spec

propNumberEnum :: Text -> [EnumOption Float] -> PropertySchema
propNumberEnum name opts = property name $ NumberEnumSchema opts

propBool :: Text -> PropertySchema
propBool name = property name BoolSchema

propArray :: Text -> ArraySpec -> PropertySchema
propArray name spec = property name $ ArraySchema spec

propObject :: Text -> ObjectSpec -> PropertySchema
propObject name spec = property name $ ObjectSchema spec

propNull :: Text -> PropertySchema
propNull name = property name NullSchema

class HasDataSchema t where
    dataSchema :: DataSchema

instance HasDataSchema Int where dataSchema = IntegerSchema def
instance HasDataSchema Integer where dataSchema = IntegerSchema def
instance HasDataSchema Natural where dataSchema = IntegerSchema def

instance HasDataSchema Float where dataSchema = NumberSchema def
instance HasDataSchema Double where dataSchema = NumberSchema def

instance HasDataSchema Bool where dataSchema = BoolSchema
instance HasDataSchema All where dataSchema = BoolSchema
instance HasDataSchema Any where dataSchema = BoolSchema

instance HasDataSchema String where dataSchema = StringSchema def
instance HasDataSchema Key where dataSchema = StringSchema def
instance HasDataSchema Text where dataSchema = StringSchema def
instance HasDataSchema TL.Text where dataSchema = StringSchema def

instance HasDataSchema UTCTime where
    dataSchema = StringSchema def { format = Just DateTimeFormat }
instance HasDataSchema LocalTime where
    dataSchema = StringSchema def { format = Just DateTimeFormat }
instance HasDataSchema TimeOfDay where
    dataSchema = StringSchema def { format = Just TimeFormat }
instance HasDataSchema DiffTime where
    dataSchema = StringSchema def { format = Just DurationFormat }

instance HasDataSchema UUID where
    dataSchema = StringSchema def { format = Just UUIDFormat }

instance HasDataSchema URI where
    dataSchema = StringSchema def { format = Just URIFormat }

instance HasDataSchema a => HasDataSchema (Maybe a) where dataSchema = dataSchema @a
instance HasDataSchema a => HasDataSchema (Identity a) where dataSchema = dataSchema @a
instance HasDataSchema a => HasDataSchema (First a) where dataSchema = dataSchema @a
instance HasDataSchema a => HasDataSchema (Last a) where dataSchema = dataSchema @a
instance HasDataSchema a => HasDataSchema (Semigroup.First a) where dataSchema = dataSchema @a
instance HasDataSchema a => HasDataSchema (Semigroup.Last a) where dataSchema = dataSchema @a
instance HasDataSchema a => HasDataSchema (Down a) where dataSchema = dataSchema @a
instance HasDataSchema a => HasDataSchema (Semigroup.Min a) where dataSchema = dataSchema @a
instance HasDataSchema a => HasDataSchema (Semigroup.Max a) where dataSchema = dataSchema @a
instance HasDataSchema a => HasDataSchema (Dual a) where dataSchema = dataSchema @a
instance HasDataSchema a => HasDataSchema (Product a) where dataSchema = dataSchema @a
instance HasDataSchema a => HasDataSchema (Sum a) where dataSchema = dataSchema @a

instance HasDataSchema IntSet where
    dataSchema = ArraySchema $ spec $ UniqueItems (IntegerSchema def)
instance HasDataSchema a => HasDataSchema [a] where
    dataSchema = ArraySchema $ spec $ ArrayItems (dataSchema @a)
instance HasDataSchema a => HasDataSchema (V.Vector a) where
    dataSchema = ArraySchema $ spec $ ArrayItems (dataSchema @a)
instance HasDataSchema a => HasDataSchema (VU.Vector a) where
    dataSchema = ArraySchema $ spec $ ArrayItems (dataSchema @a)
instance HasDataSchema a => HasDataSchema (VP.Vector a) where
    dataSchema = ArraySchema $ spec $ ArrayItems (dataSchema @a)
instance HasDataSchema a => HasDataSchema (VS.Vector a) where
    dataSchema = ArraySchema $ spec $ ArrayItems (dataSchema @a)
instance HasDataSchema a => HasDataSchema (NonEmpty a) where
    dataSchema = ArraySchema
        (spec $ ArrayItems (dataSchema @a))
            { minimumLength = Just 1 }
instance HasDataSchema a => HasDataSchema (HashSet a) where
    dataSchema = ArraySchema $ spec $ UniqueItems (dataSchema @a)

instance HasDataSchema v => HasDataSchema (KeyMap v) where
    dataSchema = ObjectSchema ObjectSpec
        { properties = []
        , additionalProperties = Just $ dataSchema @v }
instance (IsString k, HasDataSchema v) => HasDataSchema (HashMap.HashMap k v) where
    dataSchema = ObjectSchema ObjectSpec
        { properties = []
        , additionalProperties = Just $ dataSchema @v }
instance (IsString k, HasDataSchema v) => HasDataSchema (Map.Map k v) where
    dataSchema = ObjectSchema ObjectSpec
        { properties = []
        , additionalProperties = Just $ dataSchema @v }

instance (HasDataSchema a, HasDataSchema b) => HasDataSchema (a, b) where
    dataSchema = ArraySchema $ spec $
        TupleItems [dataSchema @a, dataSchema @b]
instance (HasDataSchema a, HasDataSchema b, HasDataSchema c) => HasDataSchema (a, b, c) where
    dataSchema = ArraySchema $ spec $
        TupleItems [dataSchema @a, dataSchema @b, dataSchema @c]
instance (HasDataSchema a, HasDataSchema b, HasDataSchema c, HasDataSchema d) => HasDataSchema (a, b, c, d) where
    dataSchema = ArraySchema $ spec $
        TupleItems [dataSchema @a, dataSchema @b, dataSchema @c, dataSchema @d]
instance (HasDataSchema a, HasDataSchema b, HasDataSchema c, HasDataSchema d, HasDataSchema e) => HasDataSchema (a, b, c, d, e) where
    dataSchema = ArraySchema $ spec $
        TupleItems [dataSchema @a, dataSchema @b, dataSchema @c, dataSchema @d, dataSchema @e]
instance (HasDataSchema a, HasDataSchema b, HasDataSchema c, HasDataSchema d, HasDataSchema e, HasDataSchema g) => HasDataSchema (a, b, c, d, e, g) where
    dataSchema = ArraySchema $ spec $
        TupleItems [dataSchema @a, dataSchema @b, dataSchema @c, dataSchema @d, dataSchema @e, dataSchema @g]
instance (HasDataSchema a, HasDataSchema b, HasDataSchema c, HasDataSchema d, HasDataSchema e, HasDataSchema g, HasDataSchema h) => HasDataSchema (a, b, c, d, e, g, h) where
    dataSchema = ArraySchema $ spec $
        TupleItems [dataSchema @a, dataSchema @b, dataSchema @c, dataSchema @d, dataSchema @e, dataSchema @g, dataSchema @h]
instance (HasDataSchema a, HasDataSchema b, HasDataSchema c, HasDataSchema d, HasDataSchema e, HasDataSchema g, HasDataSchema h, HasDataSchema i) => HasDataSchema (a, b, c, d, e, g, h, i) where
    dataSchema = ArraySchema $ spec $
        TupleItems [dataSchema @a, dataSchema @b, dataSchema @c, dataSchema @d, dataSchema @e, dataSchema @g, dataSchema @h, dataSchema @i]
instance (HasDataSchema a, HasDataSchema b, HasDataSchema c, HasDataSchema d, HasDataSchema e, HasDataSchema g, HasDataSchema h, HasDataSchema i, HasDataSchema j) => HasDataSchema (a, b, c, d, e, g, h, i, j) where
    dataSchema = ArraySchema $ spec $
        TupleItems [dataSchema @a, dataSchema @b, dataSchema @c, dataSchema @d, dataSchema @e, dataSchema @g, dataSchema @h, dataSchema @i, dataSchema @j]
instance (HasDataSchema a, HasDataSchema b, HasDataSchema c, HasDataSchema d, HasDataSchema e, HasDataSchema g, HasDataSchema h, HasDataSchema i, HasDataSchema j, HasDataSchema k) => HasDataSchema (a, b, c, d, e, g, h, i, j, k) where
    dataSchema = ArraySchema $ spec $
        TupleItems [dataSchema @a, dataSchema @b, dataSchema @c, dataSchema @d, dataSchema @e, dataSchema @g, dataSchema @h, dataSchema @i, dataSchema @j, dataSchema @k]

newtype RecordDefault t = RecordDefault t
newtype EnumDefault t = EnumDefault t
newtype EnumDefaultDesc t (descs :: [Symbol]) = EnumDefaultDesc t

newtype Field t = Field t
    deriving Generic
    deriving Show via t
    deriving FromJSON via t
    deriving ToJSON via t

instance Cast (Field t) t where
    cast (Field f) = f

instance Cast t (Field t) where
    cast f = Field f

newtype FieldDesc t (desc :: Symbol) = FieldDesc t
    deriving Generic
    deriving Show via t
    deriving FromJSON via t
    deriving ToJSON via t

instance Cast (FieldDesc t desc) t where
    cast (FieldDesc f) = f

instance Cast t (FieldDesc t desc) where
    cast f = FieldDesc f

class IsProperField f where
    fieldDataSchema :: DataSchema
    fieldDesc :: Maybe Text

instance HasDataSchema t => IsProperField (Field t) where
    fieldDataSchema = dataSchema @t
    fieldDesc = Nothing

instance (HasDataSchema t, KnownSymbol desc) => IsProperField (FieldDesc t desc) where
    fieldDataSchema = dataSchema @t
    fieldDesc = Just $ T.pack $ symbolVal (Proxy :: Proxy desc)

instance
    ( Generic t, HasAesonOps t
    , RecordFieldsEx IsProperField (DataSchema, Maybe Text) (Rep t) )
    => HasDataSchema (RecordDefault t) where
    dataSchema = ObjectSchema ObjectSpec
        { properties =
            let fields = recordFieldsEx @t @IsProperField \(_ :: Proxy f) -> (fieldDataSchema @f, fieldDesc @f)
            in fields & map \r ->
                let (schema, desc) = r.extra
                in PropertySchema
                    { name = T.pack $ fieldLabelModifier $ r.name
                    , description = desc
                    , schema = schema
                    , optional = tyConName (typeRepTyCon r.typeRep) == "Maybe" }
        , additionalProperties = Nothing }
        where
            opts = aesonOps @t
            fieldLabelModifier = opts.fieldLabelModifier

instance (Enum t, Bounded t, ToJSON t) => HasDataSchema (EnumDefault t) where
    dataSchema = StringEnumSchema $
        ([minBound..maxBound] :: [t]) & map \v ->
            EnumOption
                { value = TL.toStrict $ TL.decodeUtf8 $ Aeson.encode v
                , description = Nothing }

instance (Enum t, Bounded t, ToJSON t, KnownSymbols descs)
    => HasDataSchema (EnumDefaultDesc t descs) where
    dataSchema :: (Enum t, Bounded t, ToJSON t) => DataSchema
    dataSchema = StringEnumSchema $
        zip ([minBound..maxBound] :: [t]) (symbolValues @descs) & map \(v, desc) ->
            EnumOption
                { value = TL.toStrict $ TL.decodeUtf8 $ Aeson.encode v
                , description = Just $ T.pack desc }