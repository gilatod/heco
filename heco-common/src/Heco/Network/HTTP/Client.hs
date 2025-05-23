module Heco.Network.HTTP.Client where

import Network.HTTP.Client
    ( parseRequest,
      Request(requestBody, method, requestHeaders),
      RequestBody(RequestBodyLBS) )

import Data.Aeson (ToJSON)
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as T
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.CaseInsensitive (CI)

type Headers = [(CI ByteString, ByteString)]

httpRequestRaw :: ByteString -> Text -> Headers -> BSL.ByteString -> IO Request
httpRequestRaw method url headers body = do
    prevReq <- parseRequest $ T.unpack url
    pure $ prevReq
        { method = method
        --, requestBody = RequestBodyLBS $ traceShow (Aeson.encode body) $ Aeson.encode body
        , requestBody = RequestBodyLBS body
        , requestHeaders = headers }

httpRequest :: ToJSON body => ByteString -> Text -> Headers -> body -> IO Request
httpRequest method url headers body = 
    httpRequestRaw method url headers $ Aeson.encode body

httpGet :: ToJSON body => Text -> Headers -> body -> IO Request
httpGet = httpRequest "GET"

httpPost :: ToJSON body => Text -> Headers -> body -> IO Request
httpPost = httpRequest "POST"

httpDelete :: ToJSON body => Text -> Headers -> body -> IO Request
httpDelete = httpRequest "DELETE"

httpPatch :: ToJSON body => Text -> Headers -> body -> IO Request
httpPatch = httpRequest "PATCH"