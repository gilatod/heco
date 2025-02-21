module Heco.Effectful.AccountService where

import Heco.Data.Session (Session, SessionToken)
import Heco.Data.User (Username, User)

import Effectful (Effect)
import Effectful.TH (makeEffect)
import Data.Text (Text)

data LoginOps
    = UsernameLoginOps
        { username :: Text
        , password :: Text }
    | EmailLoginOps
        { email :: Text
        , password :: Text }
    deriving (Eq, Show)

data AccountService :: Effect where
    Login :: LoginOps -> AccountService m SessionToken
    Logout :: SessionToken -> AccountService m ()

    GetSessions :: AccountService m [Session]
    GetSession :: SessionToken -> AccountService m Session
    GetUser :: SessionToken -> AccountService m User

    SetUsername :: SessionToken -> Username -> AccountService m ()
    SetNickname :: SessionToken -> Text -> AccountService m ()
    SetEmail :: SessionToken -> Text -> AccountService m ()

makeEffect ''AccountService