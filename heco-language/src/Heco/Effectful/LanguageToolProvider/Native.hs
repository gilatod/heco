module Heco.Effectful.LanguageToolProvider.Native where

import Heco.Data.LanguageToolError (LanguageToolError(LanguageToolNotFoundError))
import Heco.Data.LanguageTool (AnyLanguageTool(..), invokeLanguageTool, languageToolName_, languageToolSchema_)
import Heco.Effectful.LanguageToolProvider (LanguageToolProvider(..))

import Effectful (Eff, (:>))
import Effectful.Dispatch.Dynamic (HasCallStack, interpret)
import Effectful.Error.Dynamic (Error, throwError, CallStack, runError)

import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import Data.Function ((&))

runNativeLanguageToolProvider ::
    ( HasCallStack
    , Error LanguageToolError :> es )
    => [AnyLanguageTool es]
    -> Eff (LanguageToolProvider : es) a
    -> Eff es a
runNativeLanguageToolProvider tools = interpret \_ -> \case
    GetLanguageTools -> pure schemas
    InvokeLanguageTool name args ->
        case HashMap.lookup name toolMap of
            Nothing -> throwError $ LanguageToolNotFoundError $
                "language tool not found: " ++ T.unpack name
            Just (AnyLanguageTool tool) -> invokeLanguageTool args tool
    where
        schemas = map (\(AnyLanguageTool tool) -> languageToolSchema_ tool) tools
        toolMap = HashMap.fromList $
            tools & map \rawTool@(AnyLanguageTool tool) -> (languageToolName_ tool, rawTool)

runNativeLanguageToolProviderEx ::
    HasCallStack
    => [AnyLanguageTool (Error LanguageToolError : es)]
    -> Eff (LanguageToolProvider : Error LanguageToolError : es) a
    -> Eff es (Either (CallStack, LanguageToolError) a)
runNativeLanguageToolProviderEx tools =
    runError . runNativeLanguageToolProvider tools