module Heco.Agent.AuthGroups where

import Heco.Data.AuthGroup (AuthGroup(..))

import Data.HashSet qualified as HashSet

authGroups :: [AuthGroup]
authGroups =
    [ AuthGroup
        { name = "admin"
        , description = "Administrators"
        , privileges = HashSet.empty }
    , AuthGroup
        { name = "editor"
        , description = "Editors"
        , privileges = HashSet.empty }
    , AuthGroup
        { name = "author"
        , description = "Authors"
        , privileges = HashSet.empty }
    , AuthGroup
        { name = "blocked"
        , description = "Blocked users"
        , privileges = HashSet.empty } ]