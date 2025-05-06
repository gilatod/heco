module Heco.Data.Portal.Shell where

import Heco.Data.Portal (Portal(..))
import Heco.Data.Portal (PortalProcedure)
import Heco.Data.Immanant.Terminal (TerminalSignal(..), Terminal (TerminalChat))
import Heco.Effectful.InternalTimeStream (InternalTimeStream, presentOne_)

import Effectful (Eff, (:>), IOE, MonadIO(..))
import Effectful.Concurrent (Concurrent, myThreadId, killThread)
import Effectful.Concurrent.STM
    ( atomically, newTChanIO, tryReadTChan, writeTChan, atomically )
import Effectful.Concurrent.Async (concurrently)

import Conduit (ConduitT, (.|))
import Conduit qualified as C
import Data.Conduit.List qualified as C

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

import Control.Monad (unless)
import Heco.Effectful.Ego (interactEgo, Ego)

stdinLines :: MonadIO m => ConduitT i Text m ()
stdinLines = do
    line <- liftIO T.getLine
    unless (T.null line) do
        C.yield line
        stdinLines

mergeSources ::
    (Concurrent :> es, m ~ Eff es)
    => ConduitT () o m () -> ConduitT () o m () -> ConduitT () o m ()
mergeSources src1 src2 = do
    chan <- C.lift newTChanIO
    let runSrc src = C.runConduit $ src .| C.mapM_C (atomically . writeTChan chan)
    _ <- C.lift $ concurrently (runSrc src1) (runSrc src2)
    let consume = do
            mx <- C.lift $ atomically (tryReadTChan chan)
            case mx of
                Just x  -> C.yield x >> consume
                Nothing -> pure ()
    consume

shellPortal :: forall es.
    ( IOE :> es
    , Concurrent :> es
    , InternalTimeStream :> es
    , Ego :> es )
    => Portal (Eff es)
shellPortal = Portal
    { name = "shell"
    , procedure = procedure }
    where
        procedure :: PortalProcedure (Eff es)
        procedure id sigSrc = do
            tid <- myThreadId
            C.runConduit $
                mergeSources
                    (stdinLines .| sendUserMsg id)
                    (sigSrc .| handleSigSrc tid .| C.mapM_ (liftIO . T.putStrLn))

        sendUserMsg id = C.awaitForever \s ->
            C.lift $ interactEgo do
                presentOne_ $ TerminalChat id $ "User: " <> s

        handleSigSrc tid = C.awaitForever \case
            TerminalReply msg -> C.yield msg
            TerminalClose -> C.lift $ killThread tid
            _ -> pure ()