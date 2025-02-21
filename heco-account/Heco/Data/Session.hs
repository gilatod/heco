module Heco.Data.Session where

import Data.Time (UTCTime)
import Heco.Data.User (Username)
import Data.UUID (UUID)
import Data.Hashable (Hashable)

newtype SessionToken = SessionToken UUID
    deriving (Eq, Show, Hashable)

data Session = Session
    { username :: Username
    , token :: SessionToken
    , createTime :: UTCTime }
    deriving (Show)