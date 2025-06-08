module Heco.Agent.LanguageTools.Downloader where

import Heco.Data.LanguageTool
    ( ParamDesc,
      Ret,
      LanguageTool(..),
      LanguageToolModule,
      addTool,
      emptyModuleBuilder,
      toModule )
import Heco.Data.LanguageToolError (LanguageToolError)
import Heco.Data.Task (oneShotTask, TaskId(..))
import Heco.Data.Immanant.Terminal
    ( Terminal(..), getReplyingTerminalContent )
import Heco.Effectful.HTTP (makeHttpManager)
import Heco.Effectful.InternalTimeStream (InternalTimeStream, presentOne_)
import Heco.Effectful.TaskService (TaskService, startTask, startSubTask_, killTask)
import Heco.Effectful.Agent (Agent, withAgentInteractionEx)

import Effectful ((:>), MonadIO (liftIO), IOE, Eff)
import Effectful.Error.Dynamic (Error)
import Effectful.Concurrent.Chan (Concurrent, writeChan, readChan, newChan)
import Effectful.Timeout (Timeout, timeout)
import Effectful.Exception (Exception(..), handle, SomeException(..), throwIO)

import Network.HTTP.Simple (parseRequest, getResponseBody, getResponseHeader)
import Network.HTTP.Client (Request(..), httpLbs)

import Text.Pandoc (readHtml, writeMarkdown, runIOorExplode)

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.ByteString qualified as BS
import Data.Aeson (Value)
import Data.Aeson.QQ (aesonQQ)
import Data.Aeson.Text (encodeToLazyText)
import Data.Function ((&))
import Data.Default (Default(..))

type HasDownloaderToolEs es =
    ( IOE :> es
    , Concurrent :> es
    , Timeout :> es
    , InternalTimeStream :> es
    , TaskService :> es
    , Agent :> es)

newtype DownloadException = DownloadException String
    deriving (Show)
instance Exception DownloadException

convertHtmlToMarkdown :: IOE :> es => Text -> Eff es Text
convertHtmlToMarkdown html =
    liftIO $ runIOorExplode $ readHtml def html >>= writeMarkdown def

download ::
    HasDownloaderToolEs es
    => LanguageTool es
        "download"
        "下载文件"
        (ParamDesc "url" Text "要下载的链接" -> Ret Value)
download = LanguageTool \url -> do
    chan <- newChan
    taskId <- startTask $ oneShotTask "downloader" do
        handle @SomeException (writeChan chan . Left . displayException) do
            httpMgr <- makeHttpManager Nothing
            req <- parseRequest $ T.unpack url
            let req' = req { requestHeaders = req.requestHeaders ++ headers }
            res <- timeout 10000000 $ liftIO $ httpLbs req' httpMgr
            writeChan chan $ maybe (Left "timeout") Right res

    timeout 3000000 (readChan chan) >>= \case
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

        createResponse taskRes = do
            let failedResp err = pure [aesonQQ|{ status: "failed", message: #{displayException err} }|]
            handle @SomeException failedResp do
                resp <- either (throwIO . DownloadException) pure taskRes
                content <- either throwIO pure $ TL.decodeUtf8' $ getResponseBody resp
                let content' = TL.toStrict content
                md <- case getResponseHeader "Content-Type" resp of
                    [contentType] | "text/html" `BS.isPrefixOf` contentType ->
                        convertHtmlToMarkdown content'
                    _ -> pure content'
                pure [aesonQQ|{ status: "success", content: #{md} }|]

        createNotifierTask mainTaskId sessionId chan phase =
            startSubTask_ mainTaskId $ oneShotTask "downloader-notifier" do
                resp <- TL.toStrict . encodeToLazyText <$> (readChan chan >>= createResponse)
                withAgentInteractionEx (Just phase) do
                    presentOne_ $ TerminalTaskResponse sessionId mainTaskId resp

downloaderToolModule ::
    ( HasDownloaderToolEs es
    , Error LanguageToolError :> es )
    => LanguageToolModule "downloader" (Eff es)
downloaderToolModule = emptyModuleBuilder
    & addTool download
    & toModule