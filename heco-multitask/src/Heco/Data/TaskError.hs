module Heco.Data.TaskError where
    
import Control.Exception (Exception)

data TaskError
    = TaskNotFoundError String
    deriving (Eq, Show)

instance Exception TaskError