{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Heco.Data.LanguageTool where

import Heco.Data.MonoHFunctor (MonoHFunctor(..))
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
import Data.Typeable (Typeable)
import Data.Kind (Constraint)

import GHC.TypeLits qualified as TypeLits
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal, TypeError, ErrorMessage(..))
import Pattern.Cast (Cast(..))

type family ToolNameNotConflicted (names :: [Symbol]) (name :: Symbol) :: Constraint where
    ToolNameNotConflicted '[] name = ()
    ToolNameNotConflicted (name:rst) name = 
        TypeError (TypeLits.Text "Language tool name \"" :<>: TypeLits.Text name :<>: TypeLits.Text "\" conflicted")
    ToolNameNotConflicted (_:rst) name = ToolNameNotConflicted rst name

data LanguageToolSpec m = LanguageToolSpec
    { schema :: FunctionSchema
    , handler :: HashMap Text Value -> m Value }

instance Show (LanguageToolSpec m) where
    show s = "LanguageToolSpec [" ++ show s.schema ++ "]"

instance MonoHFunctor LanguageToolSpec where
    ohmap f t = LanguageToolSpec
        { schema = t.schema
        , handler = f . t.handler }

newtype LanguageToolModule (name :: Symbol) m = LanguageToolModule
    { tools :: [LanguageToolSpec m] }
    deriving Show

instance MonoHFunctor (LanguageToolModule name) where
    ohmap f m = LanguageToolModule { tools = map (ohmap f) m.tools }

newtype LanguageToolModuleBuilder (name :: Symbol) (toolNames :: [Symbol]) m
    = LanguageToolModuleBuilder { tools :: [LanguageToolSpec m] }

emptyModuleBuilder :: forall name m. LanguageToolModuleBuilder name '[] m
emptyModuleBuilder = LanguageToolModuleBuilder { tools = [] }

toModule :: LanguageToolModuleBuilder name toolNames m -> LanguageToolModule name m
toModule builder = LanguageToolModule { tools = builder.tools }

addTool :: forall toolName toolDesc handler moduleName moduleToolNames es tool.
    ( tool ~ LanguageTool es toolName toolDesc handler
    , KnownSymbol toolName, KnownSymbol toolDesc, KnownSymbol moduleName
    , ToolNameNotConflicted moduleToolNames toolName
    , InvokableLanguageTool es tool )
    => tool
    -> LanguageToolModuleBuilder moduleName moduleToolNames (Eff es)
    -> LanguageToolModuleBuilder moduleName (toolName:moduleToolNames) (Eff es)
addTool tool m = m { tools = spec : m.tools }
    where
        moduleName = symbolText @moduleName
        rawSpec = cast tool :: LanguageToolSpec (Eff es)
        spec = LanguageToolSpec
            { schema = rawSpec.schema
                { name = moduleName <> "_" <> rawSpec.schema.name }
            , handler = rawSpec.handler }

data Param (name :: Symbol) t
data ParamDesc (name :: Symbol) t (desc :: Symbol)
data MaybeParam (name :: Symbol) t
data MaybeParamDesc (name :: Symbol) t (desc :: Symbol)
data Ret t

type family LanguageHandler es f where
    LanguageHandler es (Param name t -> b) = t -> LanguageHandler es b
    LanguageHandler es (ParamDesc name t desc -> b) = t -> LanguageHandler es b
    LanguageHandler es (MaybeParam name t -> b) = Maybe t -> LanguageHandler es b
    LanguageHandler es (MaybeParamDesc name t desc -> b) = Maybe t -> LanguageHandler es b
    LanguageHandler es (Ret r) = Eff es r

newtype LanguageTool es (name :: Symbol) (desc :: Symbol) handler
    = LanguageTool (LanguageHandler es handler)

class InvokableLanguageTool es t where
    languageToolPropertySchemas :: [PropertySchema]
    invokeLanguageTool :: HashMap Text Value -> t -> Eff es Value

data SomeLanguageTool es = forall name desc handler.
    ( InvokableLanguageTool es (LanguageTool es name desc handler)
    , KnownSymbol name, KnownSymbol desc )
    => SomeLanguageTool (LanguageTool es name desc handler)

instance
    ( InvokableLanguageTool es (LanguageTool es name desc handler)
    , KnownSymbol name, KnownSymbol desc )
    => Cast (LanguageTool es name desc handler) (SomeLanguageTool es) where
    cast = SomeLanguageTool

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
            , optional = False }
        : languageToolPropertySchemas @es @(LanguageTool es name desc handler)

    invokeLanguageTool args (LanguageTool handler) =
        case HashMap.lookup paramKey args of
            Nothing -> throwArgNotFound paramKey
            Just value ->
                case Aeson.fromJSON @t value of
                    Aeson.Error err -> throwArgInvalid paramKey err
                    Aeson.Success res -> invokeLanguageTool args
                        (LanguageTool (handler res) :: LanguageTool es name desc handler)
        where
            paramKey = symbolText @pname

instance
    ( Error LanguageToolError :> es
    , KnownSymbol pname, KnownSymbol pdesc, FromJSON t, Typeable t, HasDataSchema t
    , InvokableLanguageTool es (LanguageTool es name desc handler) )
    => InvokableLanguageTool es (LanguageTool es name desc (MaybeParamDesc pname t pdesc -> handler)) where
    languageToolPropertySchemas =
        PropertySchema
            { name = symbolText @pname
            , description = Just $ symbolText @pdesc
            , schema = dataSchema @t
            , optional = False }
        : languageToolPropertySchemas @es @(LanguageTool es name desc handler)

    invokeLanguageTool args (LanguageTool handler) =
        case HashMap.lookup paramKey args of
            Nothing -> invokeLanguageTool args
                (LanguageTool (handler Nothing) :: LanguageTool es name desc handler)
            Just value ->
                case Aeson.fromJSON @t value of
                    Aeson.Error err -> throwArgInvalid paramKey err
                    Aeson.Success res -> invokeLanguageTool args
                        (LanguageTool (handler $ Just res) :: LanguageTool es name desc handler)
        where
            paramKey = symbolText @pname

instance
    ( Error LanguageToolError :> es
    , KnownSymbol pname, FromJSON t, Typeable t, HasDataSchema t
    , InvokableLanguageTool es (LanguageTool es name desc handler) )
    => InvokableLanguageTool es (LanguageTool es name desc (MaybeParam pname t -> handler)) where
    languageToolPropertySchemas =
        PropertySchema
            { name = symbolText @pname
            , description = Nothing
            , schema = dataSchema @t
            , optional = False }
        : languageToolPropertySchemas @es @(LanguageTool es name desc handler)

    invokeLanguageTool args (LanguageTool handler) =
        case HashMap.lookup paramKey args of
            Nothing -> invokeLanguageTool args
                (LanguageTool (handler Nothing) :: LanguageTool es name desc handler)
            Just value ->
                case Aeson.fromJSON @t value of
                    Aeson.Error err -> throwArgInvalid paramKey err
                    Aeson.Success res -> invokeLanguageTool args
                        (LanguageTool (handler $ Just res) :: LanguageTool es name desc handler)
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
            , optional = False }
        : languageToolPropertySchemas @es @(LanguageTool es name desc handler)

    invokeLanguageTool args (LanguageTool handler) =
        case HashMap.lookup paramKey args of
            Nothing -> throwArgNotFound paramKey
            Just value ->
                case Aeson.fromJSON @t value of
                    Aeson.Error err -> throwArgInvalid paramKey err
                    Aeson.Success res -> invokeLanguageTool args
                        (LanguageTool (handler res) :: LanguageTool es name desc handler)
        where
            paramKey = symbolText @pname

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
        , additionalProperties = Left False } }

languageToolSchema_ :: forall t es name desc handler.
    ( t ~ LanguageTool es name desc handler
    , KnownSymbol name, KnownSymbol desc
    , InvokableLanguageTool es t )
    => t -> FunctionSchema
languageToolSchema_ _ = languageToolSchema @t

instance
    ( InvokableLanguageTool es (LanguageTool es name desc handler)
    , KnownSymbol name, KnownSymbol desc )
    => Cast (LanguageTool es name desc handler) (LanguageToolSpec (Eff es)) where
    cast tool = LanguageToolSpec
        { schema = languageToolSchema @(LanguageTool es name desc handler)
        , handler = flip invokeLanguageTool tool }