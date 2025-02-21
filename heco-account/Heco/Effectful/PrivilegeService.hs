module Heco.Effectful.PrivilegeService where

import Heco.Data.AuthGroup (AuthGroup(..), GroupName)

import Effectful (Effect, Eff)
import Effectful.TH (makeEffect)
import Effectful.Dispatch.Dynamic (reinterpret)
import Effectful.State.Static.Shared (evalState, get, modify, state)

data PrivilegeService :: Effect where
    GetAuthGroups :: PrivilegeService m [AuthGroup]
    SetAuthGroup :: AuthGroup -> PrivilegeService m ()
    RemoveAuthGroup :: GroupName -> PrivilegeService m Bool

makeEffect ''PrivilegeService

runSimplePrivilegeService ::
    [AuthGroup]
    -> Eff (PrivilegeService : es) a
    -> Eff es a
runSimplePrivilegeService initialGroups = reinterpret (evalState initialGroups) \_ -> \case
    GetAuthGroups -> get
    SetAuthGroup group -> modify \groups ->
        let (lhs, rhs) = span (\g -> g.name /= group.name) groups
        in case rhs of
            [] -> group:groups
            (_:rs) -> lhs ++ [group] ++ rs
    RemoveAuthGroup groupName -> state \groups ->
        let (lhs, rhs) = span (\g -> g.name /= groupName) groups
        in case rhs of
            [] -> (False, groups)
            (_:rs) -> (True, lhs ++ rs)