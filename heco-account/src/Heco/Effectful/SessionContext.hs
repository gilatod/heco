module Heco.Effectful.SessionContext where

import Heco.Data.Session (SessionToken, Session(..))
import Heco.Events.AccountEvent (AccountEvent(..))
import Heco.Effectful.AccountService (AccountService, getSession)
import Heco.Effectful.Event (Event, withEvent)

import Effectful (Effect, (:>), Eff, raise)
import Effectful.Dispatch.Dynamic (reinterpret)
import Effectful.State.Static.Shared (evalState, stateM, modifyM)
import Effectful.TH (makeEffect)

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Control.Monad.Extra (whenJust)

data SessionContext ctx :: Effect where
    GetSessionContext :: SessionToken -> SessionContext ctx m ctx

makeEffect ''SessionContext

runSessionContext :: forall ctx es a.
    (AccountService :> es, Event AccountEvent :> es)
    => (Session -> Eff es ctx)
    -> (Session -> ctx -> Eff es ())
    -> Eff (SessionContext ctx : es) a
    -> Eff es a
runSessionContext createCtx releaseCtx = reinterpret wrap \_ -> \case
    GetSessionContext token -> do
        session <- getSession token
        stateM \map ->
            case HashMap.lookup token map of
                Just ctx -> pure (ctx, map)
                Nothing -> do
                    ctx <- raise $ createCtx session
                    pure (ctx, HashMap.insert token ctx map)
    where
        wrap =
            evalState emptyMap
            . withEvent \case
                OnAccountLogout session -> modifyM \map -> do
                    let (ctx, map') = HashMap.alterF (\ctx -> (ctx, Nothing)) session.token map
                    whenJust ctx $ raise . releaseCtx session
                    pure map'
                _ -> pure ()
        emptyMap = HashMap.empty :: HashMap SessionToken ctx