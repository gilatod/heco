module Heco.Data.AuthGroup where

import Heco.Data.Privilege (Privilege)

import Data.Text (Text)
import Data.HashSet (HashSet, empty)
import Data.Hashable (Hashable)
import Data.String (IsString)

newtype GroupName = GroupName Text
    deriving (Eq, Show)
    deriving newtype (Hashable, IsString)

data AuthGroup = AuthGroup
    { name :: GroupName
    , description :: Text
    , privileges :: HashSet Privilege }
    deriving (Eq, Show)

emptyAuthGroup :: AuthGroup
emptyAuthGroup = AuthGroup
    { name = "empty"
    , description = "Empty authentication group"
    , privileges = empty }