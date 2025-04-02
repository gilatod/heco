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
import Data.CaseInsensitive (CI)
import Debug.Trace (traceShow)

type Headers = [(CI ByteString, ByteString)]

httpRequest :: ToJSON body => ByteString -> Text -> Headers -> body -> IO Request
httpRequest method url headers body = do
    prevReq <- parseRequest $ T.unpack url
    pure $ prevReq
        { method = method
        , requestBody = traceShow (Aeson.encode body) RequestBodyLBS $ Aeson.encode body
        , requestHeaders = headers }

httpGet :: ToJSON body => Text -> Headers -> body -> IO Request
httpGet = httpRequest "GET"

httpPost :: ToJSON body => Text -> Headers -> body -> IO Request
httpPost = httpRequest "POST"

httpDelete :: ToJSON body => Text -> Headers -> body -> IO Request
httpDelete = httpRequest "DELETE"

httpPatch :: ToJSON body => Text -> Headers -> body -> IO Request
httpPatch = httpRequest "PATCH"