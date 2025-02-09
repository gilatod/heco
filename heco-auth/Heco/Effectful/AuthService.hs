module Heco.Effectful.AuthService where

import Heco.Data.LoginConfig (LoginConfig)
import Heco.Data.Session (Session, SessionToken)

import Effectful (Effect)
import Effectful.TH (makeEffect)
import Heco.Data.AuthGroup (GroupName)
import Heco.Data.User (Username)

data AuthService :: Effect where
    Login :: LoginConfig -> AuthService m Session
    Logout :: SessionToken -> AuthService m Bool

    GetSession :: SessionToken -> AuthService m (Maybe Session)
    GetSessions :: AuthService m [Session]

    AddUserToGroup :: SessionToken -> Username -> GroupName -> AuthService m ()
    RemoveUserFromGroup :: SessionToken -> Username -> GroupName -> AuthService m ()

makeEffect ''AuthService