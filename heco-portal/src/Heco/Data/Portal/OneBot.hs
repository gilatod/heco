module Heco.Data.Portal.OneBot where

import Heco.Network.HTTP.Client (Headers, httpPost)
import Heco.Conduit.Concurrent (mergeSources)
import Heco.Data.Aeson (defaultAesonOps)
import Heco.Data.Portal (Portal(..), PortalSignal(..))
import Heco.Data.TimePhase (ImmanantContent)
import Heco.Data.TimePhase qualified as TimePhase
import Heco.Data.Immanant.Terminal (Terminal(..))
import Heco.Effectful.HTTP (makeHttpManager)
import Heco.Effectful.InternalTimeStream (InternalTimeStream, present_)
import Heco.Effectful.Ego (Ego, interactEgo)

import Effectful (Eff, IOE, type (:>), MonadIO(..))
import Effectful.Concurrent (Concurrent, myThreadId, killThread)
import Effectful.Resource (Resource)

import Conduit (ConduitT, (.|), MonadResource(..))
import Conduit qualified as C

import Network.Socket qualified as S
import Network.WebSockets qualified as WS
import Network.WebSockets.Stream qualified as WSStream
import Network.HTTP.Client (httpLbs)

import Data.Aeson qualified as Aeson
import Data.Aeson.TH (deriveJSON, deriveFromJSON, deriveToJSON)

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as T
import Data.Text.IO qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Data.Text.Lazy.Builder.Int qualified as TLB
import Data.Maybe (fromMaybe)
import Data.Vector qualified as V
import Data.Default (Default(..), def)

import System.Timeout (timeout)
import Control.Exception (throwIO)
import Control.Monad.Extra (whenJust)
import Pattern.Cast (cast)
import GHC.Generics (Generic)

data OneBotOps = OneBotOps
    { webapiUrl :: Text
    , webapiHeaders :: Headers
    , webapiTimeout :: Maybe Int
    , websocketHost :: Text
    , websocketPath :: Maybe Text
    , websocketPort :: Int
    , websocketOps :: WS.ConnectionOptions
    , websocketHeaders :: Headers
    , token :: Maybe Text }

instance Default OneBotOps where
    def = OneBotOps
        { webapiUrl = "http://localhost:3000"
        , webapiHeaders = []
        , webapiTimeout = Nothing
        , websocketHost = "localhost"
        , websocketPath = Nothing
        , websocketPort = 3001
        , websocketOps = WS.defaultConnectionOptions
        , websocketHeaders = []
        , token = Nothing }

data OneBotMessageData = OneBotMessageData
    { text :: Maybe Text
    , qq :: Maybe Text }
    deriving (Show, Generic, Default)

data OneBotMessagePart = OneBotMessagePart
    { _type :: Text
    , _data :: OneBotMessageData }
    deriving (Show)

data OneBotSender = OneBotSender
    { user_id :: Int
    , nickname :: Text
    , role :: Maybe Text }
    deriving (Show)

data OneBotEvent = OneBotMessageEvent
    { self_id :: Int
    , group_id :: Maybe Int
    , time :: Int
    , sender :: OneBotSender
    , message_type :: Text
    , message :: [OneBotMessagePart] }
    deriving (Show)

instance ImmanantContent OneBotEvent

data OneBotSendingMessage
    = OneBotSendingPrivateMessage
        { user_id :: Int
        , message :: [OneBotMessagePart] }
    | OneBotSendingGroupMessage
        { group_id :: Int
        , message :: [OneBotMessagePart] }
    deriving (Show)

deriveJSON defaultAesonOps ''OneBotMessageData
deriveJSON defaultAesonOps ''OneBotMessagePart
deriveFromJSON defaultAesonOps ''OneBotSender
deriveFromJSON defaultAesonOps ''OneBotEvent
deriveToJSON defaultAesonOps ''OneBotSendingMessage

oneBotWebsocketSource :: MonadResource m => OneBotOps -> ConduitT i OneBotEvent m ()
oneBotWebsocketSource ops = do
    addrs <- liftIO $
        S.getAddrInfo
            (Just $ S.defaultHints { S.addrSocketType = S.Stream })
            (Just $ T.unpack ops.websocketHost)
            (Just $ show port)
    addr <- case addrs of
        addr:_ -> pure addr
        [] -> liftIO $ throwIO $ WS.OtherHandshakeException "Failed to resolve address"

    sock <- liftIO $ S.socket addr.addrFamily S.Stream S.defaultProtocol
    liftIO $ S.setSocketOption sock S.NoDelay 1

    C.bracketP
        (timeout timeoutMs $ S.connect sock addr.addrAddress)
        (const $ S.close sock)
        \case
            Nothing -> liftIO $ throwIO WS.ConnectionTimeout
            Just () -> C.bracketP
                (WSStream.makeSocketStream sock)
                WSStream.close
                \stream ->
                    liftIO (WS.newClientConnection stream fullHost path ops.websocketOps headers)
                    >>= yieldEvents . liftIO . WS.receiveData
    where
        timeoutMs = ops.websocketOps.connectionTimeout * 1000 * 1000
        path = T.unpack $ fromMaybe "/" ops.websocketPath
        port = ops.websocketPort
        fullHost
            | port == 80 = T.unpack ops.websocketHost
            | otherwise =
                let builder = TLB.fromText ops.websocketHost <> ":" <> TLB.decimal port
                in TL.unpack $ TLB.toLazyText builder
        headers = ops.websocketHeaders ++
            case ops.token of
                Nothing -> []
                Just token -> [("Authorization", "Bearer " <> T.encodeUtf8 token)]
        yieldEvents m = do
            content <- m
            whenJust (Aeson.decode content) C.yield
            yieldEvents m

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
        procedure pid sigSrc = do
            tid <- myThreadId
            httpMgr <- liftIO $ makeHttpManager ops.webapiTimeout
            C.runConduit $ mergeSources
                (oneBotWebsocketSource ops .| readEvent pid)
                (sigSrc .| handleSigSrc tid httpMgr)

        readEvent pid = C.awaitForever \e -> do
            let username = e.sender.nickname
                doPresent = presentMessage pid username e
            case e.message of
                parts | e.message_type == "private" -> doPresent parts
                headPart:parts | isAtMessage e.self_id headPart -> doPresent parts
                _ -> pure ()

        presentMessage pid user event parts = do
            let builder = mconcat $ map (\m -> maybe mempty TLB.fromText m._data.text) parts
                text = TL.toStrict $ TLB.toLazyText $
                    "[onebot] " <> TLB.fromText user <>  ": " <>  builder
            liftIO $ T.putStrLn text
            C.lift $ interactEgo do
                present_ $ V.fromList
                    [ cast $ TerminalChat pid text
                    , cast event ]

        isAtMessage selfId headPart =
            headPart._type == "at" &&
                case maybe (Left "") T.decimal headPart._data.qq of
                    Left _ -> False
                    Right (qq, _) -> qq == selfId

        handleSigSrc tid httpMgr = C.awaitForever \case
            PortalReply phase msg -> do
                whenJust (TimePhase.getImmanantContent @OneBotEvent phase) \event -> do
                    liftIO $ T.putStrLn $ "[onebot] " <> event.message_type <> " / " <> msg
                    let (path, resp) = makeResponseMessage event msg
                    whenJust resp \r -> do
                        req <- liftIO $ httpPost (ops.webapiUrl <> path) apiHeaders r
                        liftIO (httpLbs req httpMgr) >> pure ()
            PortalClose -> C.lift $ killThread tid
            _ -> pure ()

        makeResponseMessage event msg =
            case event.message_type of
                "private" ->
                    ( "/send_private_msg"
                    , Just OneBotSendingPrivateMessage
                        { user_id = event.sender.user_id
                        , message = [OneBotMessagePart { _type = "text", _data = def { text = Just msg } }] })
                "group" | Just gid <- event.group_id ->
                    ( "/send_group_msg"
                    , Just OneBotSendingGroupMessage
                        { group_id = gid
                        , message =
                            let qq = TL.toStrict $ TLB.toLazyText $ TLB.decimal event.sender.user_id
                            in [ OneBotMessagePart { _type = "at", _data = def { qq = Just qq } }
                               , OneBotMessagePart { _type = "text", _data = def { text = Just $ " " <> msg } }] })
                _ -> ("", Nothing)

        apiHeaders = ops.webapiHeaders ++
            case ops.token of
                Nothing -> []
                Just token -> [("Authorization", "Bearer " <> T.encodeUtf8 token)]