module Heco.Data.AccountError where

import Data.Data (Typeable)
import Control.Exception (Exception)

data AccountError
    = InvalidSessionTokenError
    | InvalidUsernameError
    | InvalidEmailError
    | UnregisteredUserError
    | IncorrectPasswordError
    | BackendInternalError String
    | AccountBackendError String
    | UnhandledAccountError String
    deriving (Eq, Show, Typeable)

instance Exception AccountError