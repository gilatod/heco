module Heco.Data.DatabaseError where

import Control.Exception (Exception)

data DatabaseError
    = DatabaseInputError String
    | DatabaseBackendError String
    | UnhandledDatabaseError String
    deriving (Eq, Show)

instance Exception DatabaseError