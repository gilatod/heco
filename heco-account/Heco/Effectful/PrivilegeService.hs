module Heco.Effectful.PrivilegeService where

import Heco.Data.AuthGroup (AuthGroup(..), GroupName)

import Effectful (Effect, Eff)
import Effectful.TH (makeEffect)
import Effectful.Dispatch.Dynamic (reinterpret)
import Effectful.State.Static.Local (evalState, get, put)

data PrivilegeService :: Effect where
    GetAuthGroups :: PrivilegeService m [AuthGroup]
    SetAuthGroup :: AuthGroup -> PrivilegeService m ()
    RemoveAuthGroup :: GroupName -> PrivilegeService m Bool

makeEffect ''PrivilegeService

runSimplePrivilegeService ::
    [AuthGroup]
    -> Eff (PrivilegeService : es) a
    -> Eff es a
runSimplePrivilegeService groups = reinterpret (evalState groups) \_ -> \case
    GetAuthGroups -> get
    SetAuthGroup group -> do
        groups <- get
        let (lhs, rhs) = span (\g -> g.name /= group.name) groups
        case rhs of
            [] -> put (group:groups)
            (_:rs) -> do
                put $ lhs ++ [group] ++ rs
    RemoveAuthGroup groupName -> do
        groups <- get
        let (lhs, rhs) = span (\g -> g.name /= groupName) groups
        case rhs of
            [] -> pure False
            (_:rs) -> do
                put $ lhs ++ rs
                pure True