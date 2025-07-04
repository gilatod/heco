{-# LANGUAGE MultiWayIf #-}

module Heco.Agent.Tools.Downloader where

import Heco.Data.AgentTool
    ( ParamDesc,
      Ret,
      AgentTool(..),
      AgentToolModule,
      addTool,
      emptyModuleBuilder,
      toModule )
import Heco.Data.AgentToolError (AgentToolError)
import Heco.Data.Task (oneShotTask, TaskId(..))
import Heco.Data.Immanent.Terminal
    ( Terminal(..), getReplyingTerminalContent )
import Heco.Effectful.HTTP (makeHttpManager)
import Heco.Effectful.ImmanentTime (ImmanentTime, presentOne_)
import Heco.Effectful.TaskService (TaskService, startTask, startSubTask_, killTask)
import Heco.Effectful.Agent (Agent, withAgentInteractionEx)

import Effectful ((:>), MonadIO (liftIO), IOE, Eff)
import Effectful.Error.Dynamic (Error)
import Effectful.Concurrent.Chan (Concurrent, writeChan, readChan, newChan)
import Effectful.Timeout (Timeout, timeout)
import Effectful.Exception (Exception(..), handle, SomeException(..), throwIO)

import Network.HTTP.Simple (parseRequest, getResponseBody, getResponseHeader)
import Network.HTTP.Client (Request(..), httpLbs)

import Text.Pandoc (readHtml, writeMarkdown, runIOorExplode, readRTF, readDocx, readEPUB, readODT)

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Aeson (Value)
import Data.Aeson.QQ (aesonQQ)
import Data.Aeson.Text (encodeToLazyText)
import Data.Function ((&))
import Data.Default (Default(..))
import Data.Maybe (listToMaybe, fromMaybe)

import GHC.Generics (Generic)
import Effectful.Reader.Static (Reader, ask, runReader)
import Heco.Data.MonoHFunctor (MonoHFunctor(ohmap))

data DownloaderToolOps = DownloaderToolOps
    { blockingTimeout :: Maybe Float
    , taskTimeout :: Maybe Float }
    deriving (Show, Generic, Default)

type HasDownloaderToolEs es =
    ( IOE :> es
    , Concurrent :> es
    , Timeout :> es
    , ImmanentTime :> es
    , TaskService :> es
    , Agent :> es)

newtype DownloadException = DownloadException String
    deriving (Show)
instance Exception DownloadException

convertFileToMarkdown :: IOE :> es => BSL.ByteString -> BS.ByteString -> Eff es Text
convertFileToMarkdown content mime
    | "text/plain" `BS.isPrefixOf` mime =
        decodeText content
    | "text/html" `BS.isPrefixOf` mime =
        decodeText content >>= runPandoc . readHtml def
    | "application/rtf" `BS.isPrefixOf` mime =
        decodeText content >>= runPandoc . readRTF def
    | "application/vnd.openxmlformats-officedocument.wordprocessingml.document" `BS.isPrefixOf` mime =
        runPandoc $ readDocx def content
    | "application/vnd.oasis.opendocument.text" `BS.isPrefixOf` mime =
        runPandoc $ readODT def content
    | "application/epub+zip" `BS.isPrefixOf` mime =
        runPandoc $ readEPUB def content
    | otherwise =
        decodeText content
    where
        decodeText = either throwIO (pure . TL.toStrict) . TL.decodeUtf8'
        runPandoc = liftIO . runIOorExplode . (>>= writeMarkdown def)

download ::
    ( HasDownloaderToolEs es
    , Reader DownloaderToolOps :> es )
    => AgentTool es
        "download"
        "下载文件"
        (ParamDesc "url" Text "要下载的链接" -> Ret Value)
download = AgentTool \url -> do
    ops <- ask @DownloaderToolOps
    chan <- newChan
    taskId <- startTask $ oneShotTask "downloader" do
        handle @SomeException (writeChan chan . Left . displayException) do
            httpMgr <- makeHttpManager Nothing
            req <- parseRequest $ T.unpack url
            let req' = req { requestHeaders = req.requestHeaders ++ headers }
            res <- timeoutOpt ops.taskTimeout $ liftIO $ httpLbs req' httpMgr
            writeChan chan $ maybe (Left "timeout") Right res

    timeoutOpt ops.blockingTimeout (readChan chan) >>= \case
        Just resp ->
            createResponse resp
        Nothing -> getReplyingTerminalContent >>= \case
            Just (TerminalChat sessionId _, phase) -> do
                createNotifierTask taskId sessionId chan phase
                let TaskId taskIdNum = taskId
                pure [aesonQQ|{
                    status: "downloading",
                    message: "download task has been created in the background",
                    task_id: #{taskIdNum}
                }|]
            _ -> do
                killTask taskId
                pure [aesonQQ|{ status: "failed", message: "timeout" }|]
    where
        headers = [("Accept", "text/*;charset=utf-8,application/*;charset=utf-8")]

        timeoutOpt time m = case time of
            Nothing -> Just <$> m
            Just (seconds :: Float) ->
                let microseconds = floor $ 1000000 * seconds
                in timeout microseconds m

        createResponse taskRes = do
            let failedResp err = pure [aesonQQ|{ status: "failed", message: #{displayException err} }|]
            handle @SomeException failedResp do
                resp <- either (throwIO . DownloadException) pure taskRes
                markdown <- convertFileToMarkdown
                    (getResponseBody resp)
                    (fromMaybe "text/plain" . listToMaybe . getResponseHeader "Content-Type" $ resp)
                pure [aesonQQ|{ status: "success", content: #{markdown} }|]

        createNotifierTask mainTaskId sessionId chan phase =
            startSubTask_ mainTaskId $ oneShotTask "downloader-notifier" do
                resp <- TL.toStrict . encodeToLazyText <$> (readChan chan >>= createResponse)
                withAgentInteractionEx (Just phase) do
                    presentOne_ $ TerminalTaskResponse sessionId mainTaskId resp

downloaderToolModule ::
    ( HasDownloaderToolEs es
    , Error AgentToolError :> es )
    => DownloaderToolOps -> AgentToolModule "downloader" (Eff es)
downloaderToolModule ops = emptyModuleBuilder
    & addTool download
    & toModule & ohmap (runReader ops)