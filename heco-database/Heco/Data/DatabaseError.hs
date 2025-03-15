module Heco.Data.DatabaseError where

import Data.Data (Typeable)
import Control.Exception (Exception)

data DatabaseError
    = DatabaseBackendError String
    | UnhandledDatabaseError String
    deriving (Eq, Show, Typeable)

instance Exception DatabaseError