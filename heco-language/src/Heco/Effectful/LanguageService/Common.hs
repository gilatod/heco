module Heco.Effectful.LanguageService.Common where

import Heco.Data.LanguageError (LanguageError(..))
import Heco.Events.LanguageEvent (LanguageEvent(..))
import Heco.Effectful.Event (Event)

import Effectful (Eff, (:>), IOE)
import Effectful.Dispatch.Dynamic (localSeqUnliftIO, localSeqLend, LocalEnv, SharedSuffix, HasCallStack)
import Effectful.Error.Dynamic (Error, throwError)

import Network.HTTP.Client (HttpException)
import Control.Exception (SomeException, catch, Exception(displayException))

relayError :: IO a -> IO (Either LanguageError a)
relayError m = (Right <$> m)
    `catch` (\(e :: LanguageError) -> pure . Left $ e)
    `catch` (\(e :: HttpException) ->
        pure . Left . LanguageBackendError $ "HTTP error occured: " ++ displayException e)
    `catch` (\(e :: SomeException) ->
        pure . Left . UnhandledLanguageError $ displayException e)

unliftEventIO ::
    ( HasCallStack
    , SharedSuffix es handlerEs
    , IOE :> es
    , Event LanguageEvent :> es
    , Error LanguageError :> es )
    => LocalEnv localEs handlerEs
    -> ((forall r. Eff (Event LanguageEvent : localEs) r -> IO r) -> IO b)
    -> Eff es b
unliftEventIO env f = do
    res <- localSeqLend @'[Event LanguageEvent] env \useEvent ->
        localSeqUnliftIO env \unlift -> relayError $ f $ unlift . useEvent
    case res of
        Left e -> throwError e
        Right r -> pure r