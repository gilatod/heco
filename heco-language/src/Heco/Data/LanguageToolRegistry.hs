module Heco.Data.LanguageToolRegistry where

import Heco.Data.MonoHFunctor (MonoHFunctor(..))
import Heco.Data.FunctionSchema (FunctionSchema(..))
import Heco.Data.LanguageTool (LanguageToolSpec(..), LanguageToolModule(..), symbolText)

import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as HashMap
import Data.Text (Text)
import Data.Function ((&))

import GHC.TypeLits (KnownSymbol)

data LanguageToolRegistry m = LanguageToolRegistry
    { modules :: HashMap Text (HashMap Text (LanguageToolSpec m))
    , toolMap :: HashMap Text (LanguageToolSpec m)
    , schemas :: [FunctionSchema] }

instance Show (LanguageToolRegistry m) where
    show r = "LanguageToolRegistry [" ++ show r.modules ++ "]"

instance MonoHFunctor LanguageToolRegistry where
    ohmap f r = LanguageToolRegistry
        { modules = fmap (ohmap f) <$> r.modules
        , toolMap = ohmap f <$> r.toolMap
        , schemas = r.schemas }

empty :: LanguageToolRegistry m
empty = LanguageToolRegistry
    { modules = HashMap.empty
    , toolMap = HashMap.empty
    , schemas = [] }

addModule :: forall name m.
    KnownSymbol name
    => LanguageToolModule name m
    -> LanguageToolRegistry m
    -> LanguageToolRegistry m
addModule m r = LanguageToolRegistry
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
    -> LanguageToolRegistry m
    -> LanguageToolRegistry m
removeModule name r =
    case HashMap.alterF (, Nothing) name r.modules of
        (Nothing, _) -> r
        (Just tools, modules') -> LanguageToolRegistry
            { modules = modules'
            , toolMap = HashMap.difference r.toolMap tools
            , schemas = r.schemas & filter \s -> not $ HashMap.member s.name tools }