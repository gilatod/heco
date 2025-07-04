module Heco.Effectful.AgentToolProvider.Native where

import Heco.Data.MonoHFunctor (MonoHFunctor(..))
import Heco.Data.AgentToolError (AgentToolError(..))
import Heco.Data.AgentTool (AgentToolSpec(..))
import Heco.Data.AgentToolRegistry qualified as Registry
import Heco.Data.AgentToolRegistry (AgentToolRegistry(..))
import Heco.Effectful.Exception (throwUnhandled)
import Heco.Effectful.AgentToolProvider (AgentToolProvider(..))

import Effectful (Eff, (:>), Effect, raise, UnliftStrategy(..), Persistence(..), Limit(..))
import Effectful.TH (makeEffect)
import Effectful.Dispatch.Dynamic (HasCallStack, interpret, reinterpret, localLiftUnlift, localLift)
import Effectful.Error.Dynamic (Error, throwError, runErrorWith)
import Effectful.Exception (SomeException(..), catch)
import Effectful.State.Static.Shared (evalState, get, modify)

import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T

data NativeAgentToolProvider :: Effect where
    GetAgentToolRegistry :: NativeAgentToolProvider m (AgentToolRegistry m)
    ModifyLangaugeToolRegistry :: (AgentToolRegistry m -> AgentToolRegistry m) -> NativeAgentToolProvider m ()

makeEffect ''NativeAgentToolProvider

runNativeAgentToolProvider :: forall es a.
    ( HasCallStack
    , Error AgentToolError :> es )
    => Eff (AgentToolProvider : NativeAgentToolProvider : es) a
    -> Eff es a
runNativeAgentToolProvider = runNative . interpret \_ -> \case
    GetAgentToolSchemas -> do
        r <- getAgentToolRegistry
        pure r.schemas
    LookupAgentToolSchema name -> do
        r <- getAgentToolRegistry
        pure $ (\spec -> spec.schema) <$> HashMap.lookup name r.toolMap
    InvokeAgentTool name args -> do
        r <- getAgentToolRegistry
        case HashMap.lookup name r.toolMap of
            Nothing -> throwError $ AgentToolNotFoundError $
                "language tool not found: " ++ T.unpack name
            Just spec ->
                (Right <$> spec.handler args)
                `catch` \(e :: SomeException) -> pure (Left e)
    where
        runNative = reinterpret (evalState Registry.empty) \env -> \case
            GetAgentToolRegistry ->
                localLift env unliftStrategy \lift -> ohmap (lift . raise) <$> get
            ModifyLangaugeToolRegistry f ->
                localLiftUnlift env unliftStrategy \lift unlift ->
                    modify \s ->
                        let s' = f $ ohmap (lift . raise) s
                        in ohmap (evalState s . unlift) s'
        unliftStrategy = ConcUnlift Persistent Unlimited

runNativeAgentToolProviderEx ::
    HasCallStack
    => Eff (AgentToolProvider : NativeAgentToolProvider : Error AgentToolError : es) a
    -> Eff es a
runNativeAgentToolProviderEx =
    runErrorWith throwUnhandled . runNativeAgentToolProvider