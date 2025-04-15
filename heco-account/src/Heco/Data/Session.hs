module Heco.Data.Session where

import Data.Time (UTCTime)
import Heco.Data.User (Username)
import Data.Unique (Unique, hashUnique, newUnique)
import Data.Hashable (Hashable)
import Pattern.Cast (Cast(..))

newtype SessionToken = SessionToken Unique
    deriving (Eq, Ord, Hashable)

instance Show SessionToken where
    show (SessionToken u) = "SessionToken " ++ show (hashUnique u)

instance Cast SessionToken Unique where
    cast (SessionToken u) = u

newSessionToken :: IO SessionToken
newSessionToken = SessionToken <$> newUnique

data Session = Session
    { username :: Username
    , token :: SessionToken
    , createTime :: UTCTime }
    deriving (Show)