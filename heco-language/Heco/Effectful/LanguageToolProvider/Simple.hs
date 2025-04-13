module Heco.Effectful.LanguageToolProvider.Simple where

import Heco.Data.LanguageToolError (LanguageToolError (LanguageToolNotFoundError))
import Heco.Data.FunctionSchema (FunctionSchema(..))
import Heco.Effectful.LanguageToolProvider (LanguageToolProvider(..))

import Effectful (Eff, (:>))
import Effectful.Dispatch.Dynamic (HasCallStack, interpret)
import Effectful.Error.Static (Error, throwError, CallStack, runError)

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Aeson (Value)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Function ((&))

data LanguageTool es = LanguageTool
    { schema :: FunctionSchema
    , handler :: HashMap Text Value -> Eff es Value }

runSimpleLanguageToolProvider ::
    ( HasCallStack
    , Error LanguageToolError :> es )
    => [LanguageTool es]
    -> Eff (LanguageToolProvider : es) a
    -> Eff es a
runSimpleLanguageToolProvider tools = interpret \_ -> \case
    GetLanguageTools -> pure schemas
    InvokeLanguageTool name args ->
        case HashMap.lookup name toolMap of
            Nothing -> throwError $ LanguageToolNotFoundError $
                "language tool not found: " ++ T.unpack name
            Just tool -> tool.handler args
    where
        schemas = map (\t -> t.schema) tools
        toolMap = HashMap.fromList $
            tools & map \t -> (t.schema.name, t)

runSimpleLanguageToolProviderEx ::
    HasCallStack
    => [LanguageTool (Error LanguageToolError : es)]
    -> Eff (LanguageToolProvider : Error LanguageToolError : es) a
    -> Eff es (Either (CallStack, LanguageToolError) a)
runSimpleLanguageToolProviderEx tools =
    runError . runSimpleLanguageToolProvider tools