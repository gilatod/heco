module Heco.Effectful.AccountService where

import Heco.Data.LoginConfig (LoginConfig)
import Heco.Data.Session (Session, SessionToken)

import Effectful (Effect)
import Effectful.TH (makeEffect)
import Heco.Data.User (Username)
import Data.Text (Text)

data AccountService :: Effect where
    Login :: LoginConfig -> AccountService m SessionToken
    Logout :: SessionToken -> AccountService m ()

    GetSession :: SessionToken -> AccountService m Session
    GetSessions :: AccountService m [Session]

    SetUsername :: SessionToken -> Username -> AccountService m ()
    SetNickname :: SessionToken -> Text -> AccountService m ()
    SetEmail :: SessionToken -> Text -> AccountService m ()

makeEffect ''AccountService