module Heco.Data.AuthError where

data AuthError
    = InvalidUsernameError
    | InvalidEmailError
    | UnregisteredUserError
    | IncorrectPasswordError
    | BackendInternalError String
    | BackendConnectionError String
    | UnhandledAuthError String
    deriving (Eq, Show)