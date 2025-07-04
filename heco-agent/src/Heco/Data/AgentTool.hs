{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Heco.Data.AgentTool where

import Heco.Data.MonoHFunctor (MonoHFunctor(..))
import Heco.Data.FunctionSchema (FunctionSchema(..), ObjectSpec(..), PropertySchema(..), HasDataSchema(..))
import Heco.Data.AgentToolError (AgentToolError(..))

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

data AgentToolSpec m = AgentToolSpec
    { schema :: FunctionSchema
    , handler :: HashMap Text Value -> m Value }

instance Show (AgentToolSpec m) where
    show s = "AgentToolSpec [" ++ show s.schema ++ "]"

instance MonoHFunctor AgentToolSpec where
    ohmap f t = AgentToolSpec
        { schema = t.schema
        , handler = f . t.handler }

newtype AgentToolModule (name :: Symbol) m = AgentToolModule
    { tools :: [AgentToolSpec m] }
    deriving Show

instance MonoHFunctor (AgentToolModule name) where
    ohmap f m = AgentToolModule { tools = map (ohmap f) m.tools }

newtype AgentToolModuleBuilder (name :: Symbol) (toolNames :: [Symbol]) m
    = AgentToolModuleBuilder { tools :: [AgentToolSpec m] }

emptyModuleBuilder :: forall name m. AgentToolModuleBuilder name '[] m
emptyModuleBuilder = AgentToolModuleBuilder { tools = [] }

toModule :: AgentToolModuleBuilder name toolNames m -> AgentToolModule name m
toModule builder = AgentToolModule { tools = builder.tools }

addTool :: forall toolName toolDesc handler moduleName moduleToolNames es tool.
    ( tool ~ AgentTool es toolName toolDesc handler
    , KnownSymbol toolName, KnownSymbol toolDesc, KnownSymbol moduleName
    , ToolNameNotConflicted moduleToolNames toolName
    , InvokableAgentTool es tool )
    => tool
    -> AgentToolModuleBuilder moduleName moduleToolNames (Eff es)
    -> AgentToolModuleBuilder moduleName (toolName:moduleToolNames) (Eff es)
addTool tool m = m { tools = spec : m.tools }
    where
        moduleName = symbolText @moduleName
        rawSpec = cast tool :: AgentToolSpec (Eff es)
        spec = AgentToolSpec
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

newtype AgentTool es (name :: Symbol) (desc :: Symbol) handler
    = AgentTool (LanguageHandler es handler)

class InvokableAgentTool es t where
    agentToolPropertySchemas :: [PropertySchema]
    invokeAgentTool :: HashMap Text Value -> t -> Eff es Value

data SomeAgentTool es = forall name desc handler.
    ( InvokableAgentTool es (AgentTool es name desc handler)
    , KnownSymbol name, KnownSymbol desc )
    => SomeAgentTool (AgentTool es name desc handler)

instance
    ( InvokableAgentTool es (AgentTool es name desc handler)
    , KnownSymbol name, KnownSymbol desc )
    => Cast (AgentTool es name desc handler) (SomeAgentTool es) where
    cast = SomeAgentTool

throwArgNotFound ::
    Error AgentToolError :> es
    => Text -> Eff es a
throwArgNotFound key =
    throwError $ AgentToolArgumentNotFound $
        "argument is missing: " ++ T.unpack key

throwArgInvalid ::
    Error AgentToolError :> es
    => Text -> String -> Eff es a
throwArgInvalid key msg =
    throwError $ AgentToolArgumentInvalid $
        "failed to parse argument '" ++ T.unpack key ++ "': " ++ msg

symbolText :: forall s. KnownSymbol s => Text
symbolText = T.pack $ symbolVal (Proxy :: Proxy s)

instance
    ( Error AgentToolError :> es
    , KnownSymbol pname, FromJSON t, Typeable t, HasDataSchema t
    , InvokableAgentTool es (AgentTool es name desc handler) )
    => InvokableAgentTool es (AgentTool es name desc (Param pname t -> handler)) where
    agentToolPropertySchemas =
        PropertySchema
            { name = symbolText @pname
            , description = Nothing
            , schema = dataSchema @t
            , optional = False }
        : agentToolPropertySchemas @es @(AgentTool es name desc handler)

    invokeAgentTool args (AgentTool handler) =
        case HashMap.lookup paramKey args of
            Nothing -> throwArgNotFound paramKey
            Just value ->
                case Aeson.fromJSON @t value of
                    Aeson.Error err -> throwArgInvalid paramKey err
                    Aeson.Success res -> invokeAgentTool args
                        (AgentTool (handler res) :: AgentTool es name desc handler)
        where
            paramKey = symbolText @pname

instance
    ( Error AgentToolError :> es
    , KnownSymbol pname, KnownSymbol pdesc, FromJSON t, Typeable t, HasDataSchema t
    , InvokableAgentTool es (AgentTool es name desc handler) )
    => InvokableAgentTool es (AgentTool es name desc (MaybeParamDesc pname t pdesc -> handler)) where
    agentToolPropertySchemas =
        PropertySchema
            { name = symbolText @pname
            , description = Just $ symbolText @pdesc
            , schema = dataSchema @t
            , optional = False }
        : agentToolPropertySchemas @es @(AgentTool es name desc handler)

    invokeAgentTool args (AgentTool handler) =
        case HashMap.lookup paramKey args of
            Nothing -> invokeAgentTool args
                (AgentTool (handler Nothing) :: AgentTool es name desc handler)
            Just value ->
                case Aeson.fromJSON @t value of
                    Aeson.Error err -> throwArgInvalid paramKey err
                    Aeson.Success res -> invokeAgentTool args
                        (AgentTool (handler $ Just res) :: AgentTool es name desc handler)
        where
            paramKey = symbolText @pname

instance
    ( Error AgentToolError :> es
    , KnownSymbol pname, FromJSON t, Typeable t, HasDataSchema t
    , InvokableAgentTool es (AgentTool es name desc handler) )
    => InvokableAgentTool es (AgentTool es name desc (MaybeParam pname t -> handler)) where
    agentToolPropertySchemas =
        PropertySchema
            { name = symbolText @pname
            , description = Nothing
            , schema = dataSchema @t
            , optional = False }
        : agentToolPropertySchemas @es @(AgentTool es name desc handler)

    invokeAgentTool args (AgentTool handler) =
        case HashMap.lookup paramKey args of
            Nothing -> invokeAgentTool args
                (AgentTool (handler Nothing) :: AgentTool es name desc handler)
            Just value ->
                case Aeson.fromJSON @t value of
                    Aeson.Error err -> throwArgInvalid paramKey err
                    Aeson.Success res -> invokeAgentTool args
                        (AgentTool (handler $ Just res) :: AgentTool es name desc handler)
        where
            paramKey = symbolText @pname

instance
    ( Error AgentToolError :> es
    , KnownSymbol pname, KnownSymbol pdesc, FromJSON t, Typeable t, HasDataSchema t
    , InvokableAgentTool es (AgentTool es name desc handler) )
    => InvokableAgentTool es (AgentTool es name desc (ParamDesc pname t pdesc -> handler)) where
    agentToolPropertySchemas =
        PropertySchema
            { name = symbolText @pname
            , description = Just $ symbolText @pdesc
            , schema = dataSchema @t
            , optional = False }
        : agentToolPropertySchemas @es @(AgentTool es name desc handler)

    invokeAgentTool args (AgentTool handler) =
        case HashMap.lookup paramKey args of
            Nothing -> throwArgNotFound paramKey
            Just value ->
                case Aeson.fromJSON @t value of
                    Aeson.Error err -> throwArgInvalid paramKey err
                    Aeson.Success res -> invokeAgentTool args
                        (AgentTool (handler res) :: AgentTool es name desc handler)
        where
            paramKey = symbolText @pname

instance ToJSON r => InvokableAgentTool es (AgentTool es name desc (Ret r)) where
    agentToolPropertySchemas = []
    invokeAgentTool _ (AgentTool result) = Aeson.toJSON <$> result

agentToolName :: forall t es name desc handler.
    (t ~ AgentTool es name desc handler, KnownSymbol name)
    => Text
agentToolName = symbolText @name

agentToolName_ :: forall t es name desc handler.
    (t ~ AgentTool es name desc handler, KnownSymbol name)
    => t -> Text
agentToolName_ _ = symbolText @name

agentToolSchema :: forall t es name desc handler.
    ( t ~ AgentTool es name desc handler
    , KnownSymbol name, KnownSymbol desc
    , InvokableAgentTool es t )
    => FunctionSchema
agentToolSchema = FunctionSchema
    { name = symbolText @name
    , description = Just $ symbolText @desc
    , parameters = ObjectSpec
        { properties = agentToolPropertySchemas @es @t
        , additionalProperties = Left False } }

agentToolSchema_ :: forall t es name desc handler.
    ( t ~ AgentTool es name desc handler
    , KnownSymbol name, KnownSymbol desc
    , InvokableAgentTool es t )
    => t -> FunctionSchema
agentToolSchema_ _ = agentToolSchema @t

instance
    ( InvokableAgentTool es (AgentTool es name desc handler)
    , KnownSymbol name, KnownSymbol desc )
    => Cast (AgentTool es name desc handler) (AgentToolSpec (Eff es)) where
    cast tool = AgentToolSpec
        { schema = agentToolSchema @(AgentTool es name desc handler)
        , handler = flip invokeAgentTool tool }