module Heco.Data.Session where

import Data.Time (UTCTime)
import Heco.Data.User (User)
import Data.UUID (UUID)
import Data.Hashable (Hashable)

newtype SessionToken = SessionToken UUID
    deriving (Eq, Show, Hashable)

data Session = Session
    { user :: User
    , token :: SessionToken
    , createTime :: UTCTime
    , updateTime :: UTCTime }
    deriving (Show)