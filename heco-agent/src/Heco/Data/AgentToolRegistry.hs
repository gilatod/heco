module Heco.Data.AgentToolRegistry where

import Heco.Data.MonoHFunctor (MonoHFunctor(..))
import Heco.Data.FunctionSchema (FunctionSchema(..))
import Heco.Data.AgentTool (AgentToolSpec(..), AgentToolModule(..), symbolText)

import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as HashMap
import Data.Text (Text)
import Data.Function ((&))

import GHC.TypeLits (KnownSymbol)

data AgentToolRegistry m = AgentToolRegistry
    { modules :: HashMap Text (HashMap Text (AgentToolSpec m))
    , toolMap :: HashMap Text (AgentToolSpec m)
    , schemas :: [FunctionSchema] }

instance Show (AgentToolRegistry m) where
    show r = "AgentToolRegistry [" ++ show r.modules ++ "]"

instance MonoHFunctor AgentToolRegistry where
    ohmap f r = AgentToolRegistry
        { modules = fmap (ohmap f) <$> r.modules
        , toolMap = ohmap f <$> r.toolMap
        , schemas = r.schemas }

empty :: AgentToolRegistry m
empty = AgentToolRegistry
    { modules = HashMap.empty
    , toolMap = HashMap.empty
    , schemas = [] }

addModule :: forall name m.
    KnownSymbol name
    => AgentToolModule name m
    -> AgentToolRegistry m
    -> AgentToolRegistry m
addModule m r = AgentToolRegistry
    { modules = HashMap.insert moduleName toolMap r.modules
    , toolMap = HashMap.union toolMap r.toolMap
    , schemas =
        let toolSchemas = map (\s -> s.schema) tools
        in case HashMap.lookup moduleName r.modules of
            Nothing -> toolSchemas ++ r.schemas
            Just prev -> toolSchemas ++ filter (\s -> not $ HashMap.member s.name prev) r.schemas }
    where
        moduleName = symbolText @name
        tools = m.tools
        toolMap = HashMap.fromList $ map (\s -> (s.schema.name, s)) tools

removeModule ::
    Text
    -> AgentToolRegistry m
    -> AgentToolRegistry m
removeModule name r =
    case HashMap.alterF (, Nothing) name r.modules of
        (Nothing, _) -> r
        (Just tools, modules') -> AgentToolRegistry
            { modules = modules'
            , toolMap = HashMap.difference r.toolMap tools
            , schemas = r.schemas & filter \s -> not $ HashMap.member s.name tools }