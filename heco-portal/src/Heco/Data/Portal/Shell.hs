module Heco.Data.Portal.Shell where

import Heco.Conduit.Concurrent (mergeSources)
import Heco.Data.Portal (Portal(..), PortalSignal(..))
import Heco.Data.TimePhase (format)
import Heco.Data.Immanant.Terminal (Terminal(..))
import Heco.Effectful.InternalTimeStream (InternalTimeStream, presentOne_, getRetention)
import Heco.Effectful.Ego (interactEgo, Ego)

import Effectful (Eff, (:>), IOE)
import Effectful.Concurrent (Concurrent)

import Conduit (ConduitT, (.|), MonadIO(..))
import Conduit qualified as C

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy.IO qualified as TL
import Data.Vector qualified as V

import Control.Monad (unless)
import Heco.Effectful.PortalService (PortalService, killPortal_)

stdinLines :: MonadIO m => ConduitT i Text m ()
stdinLines = do
    line <- liftIO T.getLine
    unless (T.null line) do
        C.yield line
        stdinLines

shellPortal :: forall es.
    ( IOE :> es
    , Concurrent :> es
    , InternalTimeStream :> es
    , PortalService :> es
    , Ego :> es )
    => Portal (Eff es)
shellPortal = Portal
    { name = "shell"
    , procedure = procedure }
    where
        procedure pid sigSrc =
            C.runConduit $ mergeSources
                (stdinLines .| handleUserInput pid)
                (sigSrc .| handleSigSrc pid)

        handleUserInput pid = C.awaitForever \case
            "/history" -> do
                retention <- C.lift getRetention
                V.forM_ retention $ liftIO . TL.putStrLn . format
            "/quit" -> do
                C.lift $ killPortal_ pid
            s -> if T.head s == '/'
                then liftIO $ putStrLn $ "Error: invalid command "  <> T.unpack s
                else C.lift $ interactEgo do
                    presentOne_ $ TerminalChat pid $ "User: " <> s

        handleSigSrc pid = C.awaitForever \case
            PortalReply _ msg -> liftIO $ T.putStrLn msg
            PortalClose -> C.lift $ killPortal_ pid
            _ -> pure ()