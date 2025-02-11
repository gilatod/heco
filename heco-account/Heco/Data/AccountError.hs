module Heco.Data.AccountError where
import Data.Data (Typeable)
import Control.Exception (Exception)

data AccountError
    = InvalidSessionToken
    | InvalidUsernameError
    | InvalidEmailError
    | UnregisteredUserError
    | IncorrectPasswordError
    | BackendInternalError String
    | BackendOperationError String
    | UnhandledAccountError String
    deriving (Eq, Show, Typeable)

instance Exception AccountError