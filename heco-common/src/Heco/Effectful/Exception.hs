module Heco.Effectful.Exception where

import Effectful (Eff)
import Effectful.Exception (throwIO, Exception(displayException))
import Effectful.Error.Dynamic (CallStack, HasCallStack)

import Data.List (intersperse)
import GHC.Exception (prettyCallStackLines)

data UnhandledHecoException =
    forall e. Exception e => UnhandledHecoException e CallStack

instance Show UnhandledHecoException where
    show (UnhandledHecoException e callStack) = concat $
        [ "Unhandled exception: ", displayException e, "\n  " ]
            ++ intersperse "\n  " (prettyCallStackLines callStack)

instance Exception UnhandledHecoException

throwUnhandled :: (HasCallStack, Exception e) => CallStack -> e -> Eff es a
throwUnhandled c e = throwIO $ UnhandledHecoException e c