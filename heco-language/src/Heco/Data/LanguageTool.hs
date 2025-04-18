{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Heco.Data.LanguageTool where

import Heco.Data.FunctionSchema (FunctionSchema(..), ObjectSpec(..), PropertySchema(..), HasDataSchema(..))
import Heco.Data.LanguageToolError (LanguageToolError(..))

import Effectful (Eff, (:>))
import Effectful.Error.Dynamic (Error, throwError)

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap

import Data.Aeson (Value, FromJSON, ToJSON)
import Data.Aeson qualified as Aeson

import Data.Text (Text)
import Data.Text qualified as T
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable, typeRepTyCon, tyConName, typeRep)

import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Pattern.Cast (Cast(..))

data LanguageToolSpec es = LanguageToolSpec
    { schema :: FunctionSchema
    , handler :: HashMap Text Value -> Eff es Value }

data Param (name :: Symbol) t
data ParamDesc (name :: Symbol) t (desc :: Symbol)
data Ret t

type family LanguageHandler es f where
    LanguageHandler es (Param name t -> b) = t -> LanguageHandler es b
    LanguageHandler es (ParamDesc name t desc -> b) = t -> LanguageHandler es b
    LanguageHandler es (Ret r) = Eff es r

newtype LanguageTool es (name :: Symbol) (desc :: Symbol) handler = LanguageTool (LanguageHandler es handler)

class InvokableLanguageTool es t where
    languageToolPropertySchemas :: [PropertySchema]
    invokeLanguageTool :: HashMap Text Value -> t -> Eff es Value

data AnyLanguageTool es = forall name desc handler.
    ( InvokableLanguageTool es (LanguageTool es name desc handler)
    , KnownSymbol name, KnownSymbol desc )
    => AnyLanguageTool (LanguageTool es name desc handler)

instance
    ( InvokableLanguageTool es (LanguageTool es name desc handler)
    , KnownSymbol name, KnownSymbol desc )
    => Cast (LanguageTool es name desc handler) (AnyLanguageTool es) where
    cast = AnyLanguageTool

throwArgNotFound ::
    Error LanguageToolError :> es
    => Text -> Eff es a
throwArgNotFound key =
    throwError $ LanguageToolArgumentNotFound $
        "argument is missing: " ++ T.unpack key

throwArgInvalid ::
    Error LanguageToolError :> es
    => Text -> String -> Eff es a
throwArgInvalid key msg =
    throwError $ LanguageToolArgumentInvalid $
        "failed to parse argument '" ++ T.unpack key ++ "': " ++ msg

isMaybeType :: forall t. Typeable t => Bool
isMaybeType =
    let conName = tyConName $ typeRepTyCon $ typeRep (Proxy :: Proxy t)
    in conName == "Maybe"

symbolText :: forall s. KnownSymbol s => Text
symbolText = T.pack $ symbolVal (Proxy :: Proxy s)

instance
    ( Error LanguageToolError :> es
    , KnownSymbol pname, FromJSON t, Typeable t, HasDataSchema t
    , InvokableLanguageTool es (LanguageTool es name desc handler) )
    => InvokableLanguageTool es (LanguageTool es name desc (Param pname t -> handler)) where
    languageToolPropertySchemas =
        PropertySchema
            { name = symbolText @pname
            , description = Nothing
            , schema = dataSchema @t
            , optional = isMaybeType @t }
        : languageToolPropertySchemas @es @(LanguageTool es name desc handler)

    invokeLanguageTool args (LanguageTool handler) =
        case HashMap.lookup paramKey args of
            Nothing -> throwArgNotFound paramKey
            Just value ->
                case Aeson.fromJSON @t value of
                    Aeson.Error err -> throwArgInvalid paramKey err
                    Aeson.Success res -> invokeLanguageTool args $
                        (LanguageTool (handler res) :: LanguageTool es name desc handler)
        where
            paramKey = symbolText @pname

instance
    ( Error LanguageToolError :> es
    , KnownSymbol pname, KnownSymbol pdesc, FromJSON t, Typeable t, HasDataSchema t
    , InvokableLanguageTool es (LanguageTool es name desc handler) )
    => InvokableLanguageTool es (LanguageTool es name desc (ParamDesc pname t pdesc -> handler)) where
    languageToolPropertySchemas =
        PropertySchema
            { name = symbolText @pname
            , description = Just $ symbolText @pdesc
            , schema = dataSchema @t
            , optional = isMaybeType @t }
        : languageToolPropertySchemas @es @(LanguageTool es name desc handler)

    invokeLanguageTool args (LanguageTool handler) =
        case HashMap.lookup paramKey args of
            Nothing -> throwArgNotFound paramKey
            Just value ->
                case Aeson.fromJSON @t value of
                    Aeson.Error err -> throwArgInvalid paramKey err
                    Aeson.Success res -> invokeLanguageTool args $
                        (LanguageTool (handler res) :: LanguageTool es name desc handler)
        where
            paramKey = T.pack $ symbolVal (Proxy :: Proxy pname)

instance ToJSON r => InvokableLanguageTool es (LanguageTool es name desc (Ret r)) where
    languageToolPropertySchemas = []
    invokeLanguageTool _ (LanguageTool result) = Aeson.toJSON <$> result

languageToolName :: forall t es name desc handler.
    (t ~ LanguageTool es name desc handler, KnownSymbol name)
    => Text
languageToolName = symbolText @name

languageToolName_ :: forall t es name desc handler.
    (t ~ LanguageTool es name desc handler, KnownSymbol name)
    => t -> Text
languageToolName_ _ = symbolText @name

languageToolSchema :: forall t es name desc handler.
    ( t ~ LanguageTool es name desc handler
    , KnownSymbol name, KnownSymbol desc
    , InvokableLanguageTool es t )
    => FunctionSchema
languageToolSchema = FunctionSchema
    { name = symbolText @name
    , description = Just $ symbolText @desc
    , parameters = ObjectSpec
        { properties = languageToolPropertySchemas @es @t
        , additionalProperties = Nothing } }

languageToolSchema_ :: forall t es name desc handler.
    ( t ~ LanguageTool es name desc handler
    , KnownSymbol name, KnownSymbol desc
    , InvokableLanguageTool es t )
    => t -> FunctionSchema
languageToolSchema_ _ = languageToolSchema @t

instance
    ( InvokableLanguageTool es (LanguageTool es name desc handler)
    , KnownSymbol name, KnownSymbol desc )
    => Cast (LanguageTool es name desc handler) (LanguageToolSpec es) where
    cast tool = LanguageToolSpec
        { schema = languageToolSchema @(LanguageTool es name desc handler)
        , handler = flip invokeLanguageTool tool }