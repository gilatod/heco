module Heco.Data.Portal.OneBot where

import Heco.Network.HTTP.Client (Headers)
import Heco.Conduit.Concurrent (mergeSources)
import Heco.Data.Portal (Portal(..), PortalSignalSource, PortalSignal(..))
import Heco.Data.Immanant.Terminal (Terminal(..), TerminalId)
import Heco.Effectful.InternalTimeStream (InternalTimeStream)
import Heco.Effectful.Ego (Ego)

import Effectful ( Eff, IOE, type (:>))
import Effectful.Concurrent (Concurrent, myThreadId, killThread)

import Conduit (ConduitT, (.|), MonadIO(..), MonadResource(..))
import Conduit qualified as C

import Network.Socket qualified as S
import Network.WebSockets qualified as WS
import Network.WebSockets.Stream qualified as WSStream

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Data.Text.Lazy.Builder.Int qualified as TLB
import Data.Maybe (fromMaybe)

import System.Timeout (timeout)
import Control.Exception (throwIO)
import Effectful.Resource (Resource)

data OneBotOps = OneBotOps
    { host :: Text
    , webapiPath :: Maybe Text
    , webapiPort :: Int
    , webapiHeaders :: Headers
    , websocketPath :: Maybe Text
    , websocketPort :: Int
    , websocketOps :: WS.ConnectionOptions
    , websocketHeaders :: Headers
    , token :: Maybe Text }

oneBotOps :: Text -> OneBotOps
oneBotOps host = OneBotOps
    { host = host
    , webapiPath = Nothing
    , webapiPort = 3000
    , webapiHeaders = []
    , websocketPath = Nothing
    , websocketPort = 3001
    , websocketOps = WS.defaultConnectionOptions
    , websocketHeaders = []
    , token = Nothing }

sourceOneBotWebsocket :: MonadResource m => OneBotOps -> ConduitT i Text m ()
sourceOneBotWebsocket ops = do
    addrs <- liftIO $
        S.getAddrInfo
            (Just $ S.defaultHints { S.addrSocketType = S.Stream })
            (Just $ T.unpack ops.host)
            (Just $ show port)
    addr <- case addrs of
        addr:_ -> pure addr
        [] -> liftIO $ throwIO $ WS.OtherHandshakeException "Failed to resolve address"

    sock <- liftIO $ S.socket addr.addrFamily S.Stream S.defaultProtocol
    liftIO $ S.setSocketOption sock S.NoDelay 1

    C.bracketP 
        (timeout timeoutMs $ S.connect sock addr.addrAddress)
        (const $ S.close sock) $
        \case
            Nothing -> liftIO $ throwIO $ WS.ConnectionTimeout
            Just () -> C.bracketP
                (WSStream.makeSocketStream sock)
                WSStream.close
                \stream ->
                    liftIO (WS.newClientConnection stream fullHost path ops.websocketOps headers)
                    >>= C.repeatMC . liftIO . WS.receiveData
    where
        timeoutMs = ops.websocketOps.connectionTimeout * 1000 * 1000
        path = T.unpack $ fromMaybe "/" ops.websocketPath
        port = ops.websocketPort
        fullHost =
            if port == 80
                then T.unpack ops.host
                else TL.unpack $ TLB.toLazyText $
                    TLB.fromText ops.host <> ":" <> TLB.decimal port
        headers = ops.websocketHeaders ++
            case ops.token of
                Nothing -> []
                Just token -> [("Authorization", "Bearer " <> T.encodeUtf8 token)]

makeOneBotPortal :: forall es.
    ( IOE :> es
    , Resource :> es
    , Concurrent :> es
    , InternalTimeStream :> es
    , Ego :> es )
    => OneBotOps -> Portal (Eff es)
makeOneBotPortal ops = Portal
    { name = "onebot"
    , procedure = procedure }
    where
        procedure id sigSrc = do
            tid <- myThreadId
            C.runConduit $ mergeSources
                (sourceOneBotWebsocket ops .| C.awaitForever (liftIO . T.putStrLn))
                (sigSrc .| handleSigSrc tid)

        handleSigSrc tid = C.awaitForever \case
            PortalReply msg -> liftIO $ T.putStrLn msg
            PortalClose -> C.lift $ killThread tid