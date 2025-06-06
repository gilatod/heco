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
import Heco.Effectful.InternalTimeStream (InternalTimeStream, presentOne_)
import Heco.Effectful.TaskService (TaskService, startTask, startSubTask_, killTask)
import Heco.Effectful.Agent (withAgentInteraction, Agent, withAgentInteractionEx)

import Effectful ((:>), MonadIO (liftIO), IOE, Eff)
import Effectful.Error.Dynamic (Error)
import Effectful.Concurrent.Chan (Concurrent, writeChan, readChan, newChan)
import Effectful.Timeout (Timeout, timeout)
import Effectful.Exception (Exception(..), handle, SomeException(..))

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.Aeson (Value)
import Data.Aeson.QQ (aesonQQ)
import Data.Aeson.Text (encodeToLazyText)
import Data.Function ((&))

import Network.HTTP.Simple (parseRequest, getResponseBody, httpLBS)
import Pattern.Cast (Cast(..))
import Effectful.Concurrent (threadDelay)
import Debug.Trace (traceShow)

type HasDownloaderToolEs es =
    ( IOE :> es
    , Concurrent :> es
    , Timeout :> es
    , InternalTimeStream :> es
    , TaskService :> es
    , Agent :> es )

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
            req <- parseRequest $ T.unpack url
            res <- timeout 10000000 $ liftIO $ httpLBS req
            writeChan chan $ maybe (Left "timeout") Right res

    timeout 3000000 (readChan chan) >>= \case
        Just resp ->
            pure $ createResponse resp
        Nothing -> getReplyingTerminalContent >>= \case
            Just (TerminalChat sessionId _, phase) -> do
                createNotifierTask taskId sessionId chan phase
                let taskIdNum = cast @TaskId @Int taskId
                pure [aesonQQ|{
                    status: "downloading",
                    message: "download task has been created in the background",
                    task_id: #{taskIdNum}
                }|]
            _ -> do
                killTask taskId
                pure [aesonQQ|{ status: "failed", message: "timeout" }|]
    where
        createResponse = \case
            Right r ->
                let content = TL.decodeUtf8 $ getResponseBody r
                in [aesonQQ|{ status: "success", content: #{content} }|]
            Left err ->
                [aesonQQ|{ status: "failed", message: #{err} }|]

        createNotifierTask mainTaskId sessionId chan phase =
            startSubTask_ mainTaskId $ oneShotTask "downloader-notifier" do
                resp <- TL.toStrict . encodeToLazyText . createResponse <$> readChan chan
                withAgentInteractionEx (Just phase) do
                    presentOne_ $ TerminalTaskResponse sessionId mainTaskId resp

downloaderToolModule ::
    ( HasDownloaderToolEs es
    , Error LanguageToolError :> es )
    => LanguageToolModule "downloader" (Eff es)
downloaderToolModule = emptyModuleBuilder
    & addTool download
    & toModule