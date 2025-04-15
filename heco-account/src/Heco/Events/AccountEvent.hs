module Heco.Events.AccountEvent where

import Heco.Data.Session (Session)
import Heco.Data.User (Username)
import Data.Text (Text)

data AccountEvent
    = OnAccountLogin Session
    | OnAccountLogout Session
    | OnAccountUsernameChanged Session Username
    | OnAccountNicknameChanged Session Text
    | OnAccountEmailChanged Session Text