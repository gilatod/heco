module Heco.Data.Privilege where

data Privilege
    = ChatPrivilege
    | UserCreatePrivilege
    | UserDeletePrivilege
    deriving (Eq, Show)