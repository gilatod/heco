module Heco.Effectful.SessionContext where

import Heco.Data.Session (SessionToken, Session(..))
import Heco.Events.AccountEvent (AccountEvent(..))
import Heco.Effectful.AccountService (AccountService, getSession)
import Heco.Effectful.Event (Event, listen_)

import Effectful (Effect, (:>), Eff, raise)
import Effectful.Dispatch.Dynamic (reinterpret)
import Effectful.State.Static.Shared (evalState, stateM, modifyM)
import Effectful.TH (makeEffect)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

data SessionContext ctx :: Effect where
    GetSessionContext :: SessionToken -> SessionContext ctx m ctx

makeEffect ''SessionContext

runSessionContext :: forall ctx es a.
    (AccountService :> es, Event AccountEvent :> es)
    => (Session -> Eff es ctx)
    -> (Session -> ctx -> Eff es ())
    -> Eff (SessionContext ctx : es) a
    -> Eff es a
runSessionContext createCtx releaseCtx = reinterpret evalContextState \_ -> \case
    GetSessionContext token -> do
        session <- getSession token
        stateM \map ->
            case HashMap.lookup token map of
                Just ctx -> pure (ctx, map)
                Nothing -> do
                    ctx <- raise $ createCtx session
                    pure (ctx, HashMap.insert token ctx map)
    where
        evalContextState e = evalState emptyMap do
            listen_ \case
                OnAccountLogout session -> modifyM \map -> do
                    let (ctx, map') = HashMap.alterF (\ctx -> (ctx, Nothing)) session.token map
                    maybe (pure ()) (raise . releaseCtx session) ctx
                    pure map'
                _ -> pure()
            e
        emptyMap = HashMap.empty :: HashMap SessionToken ctx