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
import Heco.Effectful.TaskService (TaskService)

import Effectful ((:>), MonadIO (liftIO), IOE, Eff)
import Effectful.Error.Dynamic (Error)
import Effectful.Concurrent.Chan (Concurrent)
import Effectful.Timeout (Timeout, timeout)

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy.Encoding qualified as TL
import Data.Aeson (Value)
import Data.Aeson.QQ (aesonQQ)
import Data.Function ((&))

import Network.HTTP.Simple (parseRequest, getResponseBody)
import Network.HTTP.Client (httpLbs)

import Heco.Effectful.HTTP (makeHttpManager)

download ::
    ( IOE :> es
    , Timeout :> es )
    => LanguageTool es
        "download"
        "下载文件"
        (ParamDesc "url" Text "要下载的链接" -> Ret Value)
download = LanguageTool \url -> do
    resp <- timeout 10000000 $ liftIO do
        manager <- makeHttpManager Nothing
        req <- parseRequest $ T.unpack url
        httpLbs req manager
    pure case resp of
        Nothing -> [aesonQQ|{ status: "timeout" }|]
        Just r ->
            let content = TL.decodeUtf8 $ getResponseBody r
            in [aesonQQ|{ status: "success", content: #{content} }|]

    -- chan <- newChan
    -- startTask_ $ oneShotTask "downloader" do
    --     req <- parseRequest $ T.unpack url
    --     res <- liftIO $ httpBS req
    --     writeChan chan res

    -- resp <- timeout 1000000 $ readChan chan
    -- case resp of 
    --     Nothing ->
            
    -- pure undefined

downloaderToolModule ::
    ( IOE :> es
    , Concurrent :> es
    , Timeout :> es
    , TaskService :> es
    , Error LanguageToolError :> es )
    => LanguageToolModule "downloader" (Eff es)
downloaderToolModule = emptyModuleBuilder
    & addTool download
    & toModule