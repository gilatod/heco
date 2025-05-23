module Heco.Effectful.LanguageToolProvider.Native where

import Heco.Data.LanguageToolError (LanguageToolError(LanguageToolNotFoundError))
import Heco.Data.FunctionSchema (FunctionSchema(..))
import Heco.Data.LanguageTool (LanguageToolSpec(..))
import Heco.Effectful.LanguageToolProvider (LanguageToolProvider(..))

import Effectful (Eff, (:>))
import Effectful.Dispatch.Dynamic (HasCallStack, interpret)
import Effectful.Error.Dynamic (Error, throwError, CallStack, runError)

import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import Data.Function ((&))
import Effectful.Exception (SomeException(..), catch)

runNativeLanguageToolProvider ::
    ( HasCallStack
    , Error LanguageToolError :> es )
    => [LanguageToolSpec es]
    -> Eff (LanguageToolProvider : es) a
    -> Eff es a
runNativeLanguageToolProvider tools = interpret \_ -> \case
    GetLanguageTools -> pure schemas
    InvokeLanguageTool name args ->
        case HashMap.lookup name toolMap of
            Nothing -> throwError $ LanguageToolNotFoundError $
                "language tool not found: " ++ T.unpack name
            Just spec ->
                (Right <$> spec.handler args)
                `catch` \(e :: SomeException) -> pure (Left e)
                    
    where
        schemas = map (\spec -> spec.schema) tools
        toolMap = HashMap.fromList $
            tools & map \spec -> (spec.schema.name, spec)

runNativeLanguageToolProviderEx ::
    HasCallStack
    => [LanguageToolSpec (Error LanguageToolError : es)]
    -> Eff (LanguageToolProvider : Error LanguageToolError : es) a
    -> Eff es (Either (CallStack, LanguageToolError) a)
runNativeLanguageToolProviderEx tools =
    runError . runNativeLanguageToolProvider tools