module Heco.Data.DatabaseError where

import Data.Data (Typeable)
import Control.Exception (Exception)

data DatabaseError
    = DatabaseInputError String
    | DatabaseBackendError String
    | UnhandledDatabaseError String
    deriving (Eq, Show, Typeable)

instance Exception DatabaseError