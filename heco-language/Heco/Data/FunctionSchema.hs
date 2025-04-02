{-# LANGUAGE DeriveAnyClass #-}

module Heco.Data.FunctionSchema where

import Data.Text (Text)
import Data.Default (Default (..))
import GHC.Generics (Generic)

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