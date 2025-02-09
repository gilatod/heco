module Heco.Data.LoginConfig where

import Data.Text (Text)

data LoginConfig
    = UsernameLoginConfig
        { username :: Text
        , password :: Text }
    | EmailLoginConfig
        { email :: Text
        , password :: Text }
    deriving (Eq, Show)