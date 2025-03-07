module Heco.Data.Session where

import Data.Time (UTCTime)
import Heco.Data.User (Username)
import Data.UUID (UUID)
import Data.Hashable (Hashable)
import Pattern.Cast (Cast(..))

newtype SessionToken = SessionToken UUID
    deriving (Eq, Show, Hashable)

instance Cast SessionToken UUID where
    cast (SessionToken id) = id

data Session = Session
    { username :: Username
    , token :: SessionToken
    , createTime :: UTCTime }
    deriving (Show)