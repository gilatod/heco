module Heco.Effectful.Exception where

import Effectful (Eff)
import Effectful.Exception (throwIO, Exception)
import Effectful.Error.Dynamic (CallStack, HasCallStack)
import Data.Data (Typeable)
import GHC.Exception (prettyCallStackLines)
import Data.List (intersperse)

data HecoUnhandledException =
    forall e. Exception e => HecoUnhandledException e CallStack
    deriving (Typeable)

instance Show HecoUnhandledException where
    show (HecoUnhandledException e callStack) = concat $
        [ "Unhandled exception: ", show e, "\n  " ]
            ++ intersperse "\n  " (prettyCallStackLines callStack)

instance Exception HecoUnhandledException

eitherThrowIO :: (HasCallStack, Exception e) => Eff es (Either (CallStack, e) a) -> Eff es a
eitherThrowIO m =
    m >>= either throw pure
    where
        throw (callStack, e) =
            throwIO $ HecoUnhandledException e callStack