module Heco.Effectful.LanguageToolProvider.Native where

import Heco.Data.MonoHFunctor (MonoHFunctor(..))
import Heco.Data.LanguageToolError (LanguageToolError(LanguageToolNotFoundError))
import Heco.Data.LanguageTool (LanguageToolSpec(..))
import Heco.Data.LanguageToolRegistry qualified as Registry
import Heco.Data.LanguageToolRegistry (LanguageToolRegistry(..))
import Heco.Effectful.LanguageToolProvider (LanguageToolProvider(..))

import Effectful (Eff, (:>), Effect, raise, UnliftStrategy (SeqUnlift))
import Effectful.Dispatch.Dynamic (HasCallStack, interpret, reinterpret, localSeqLift, localLiftUnlift)
import Effectful.Error.Dynamic (Error, throwError, CallStack, runError)
import Effectful.Exception (SomeException(..), catch)
import Effectful.State.Static.Shared (evalState, get, modify)

import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import Effectful.TH (makeEffect)

data NativeLanguageToolProvider :: Effect where
    GetLanguageToolRegistry :: forall m. NativeLanguageToolProvider m (LanguageToolRegistry m)
    ModifyLangaugeToolRegistry :: (LanguageToolRegistry m -> LanguageToolRegistry m) -> NativeLanguageToolProvider m ()

makeEffect ''NativeLanguageToolProvider

runNativeLanguageToolProvider :: forall es a.
    ( HasCallStack
    , Error LanguageToolError :> es )
    => Eff (LanguageToolProvider : NativeLanguageToolProvider : es) a
    -> Eff es a
runNativeLanguageToolProvider = runNative . interpret \_ -> \case
    GetLanguageToolSchemas -> do
        r <- getLanguageToolRegistry
        pure r.schemas
    LookupLanguageToolSchema name -> do
        r <- getLanguageToolRegistry
        pure $ (\spec -> spec.schema) <$> HashMap.lookup name r.toolMap
    InvokeLanguageTool name args -> do
        r <- getLanguageToolRegistry
        case HashMap.lookup name r.toolMap of
            Nothing -> throwError $ LanguageToolNotFoundError $
                "language tool not found: " ++ T.unpack name
            Just spec ->
                (Right <$> spec.handler args)
                `catch` \(e :: SomeException) -> pure (Left e)
    where
        runNative = reinterpret (evalState Registry.empty) \env -> \case
            GetLanguageToolRegistry ->
                localSeqLift env \lift -> ohmap (lift . raise) <$> get
            ModifyLangaugeToolRegistry f ->
                localLiftUnlift env SeqUnlift \lift unlift ->
                    modify \s ->
                        let s' = f $ ohmap (lift . raise) s
                        in ohmap (evalState undefined . unlift) s'

runNativeLanguageToolProviderEx ::
    ( HasCallStack
    , fullEs ~ Error LanguageToolError : es )
    => Eff (LanguageToolProvider : NativeLanguageToolProvider : fullEs) a
    -> Eff es (Either (CallStack, LanguageToolError) a)
runNativeLanguageToolProviderEx =
    runError . runNativeLanguageToolProvider