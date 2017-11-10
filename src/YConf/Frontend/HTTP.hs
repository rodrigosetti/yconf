{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : YConf.Frontend.HTTP
Description : HTTP frontend implementation
Copyright   : Copyright 2017, Yahoo Holdings Inc.
License     : Licensed under the terms of the BSD3 license. See LICENSE file for terms.
Maintainer  : rodrigosetti@gmail.com
Stability   : experimental
Portability : POSIX

Implementation of a Restful HTTP server that returns the configuration data as
JSON. The context is encoded via query-strings, and the path is interpreted as
a "configuration path" (sort of JSON path) to extract sub-configuration
objects.
-}
module YConf.Frontend.HTTP ( HTTP
                           , httpFrontend) where

import           Data.Aeson                   hiding ((.=))
import           Data.Aeson.Types             hiding ((.=))
import qualified Data.HashMap.Strict          as H
import           Data.Maybe
import           Data.Scientific
import           Data.String
import qualified Data.Text                    as T
import           Data.Text.Encoding
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Gzip
import           Network.Wai.Middleware.Jsonp
import           System.Logger                hiding (Settings)
import           YConf.Frontend
import           YConf.Frontend.Helpers
import           YConf.Live
import           YConf.Tree
import           YConf.Types

newtype HTTP = HTTP { httpSettings :: Settings }

httpFrontend :: String -> HTTP
httpFrontend bind =
    HTTP $ case break (== ':') bind of
             (port, "")       -> setPort (read port) defaultSettings
             (host, ':':port) -> setPort (read port) $ setHost (fromString host) defaultSettings
             _                -> error $ "invalid configuration for HTTP frontend: " ++ bind

instance Frontend HTTP where

    runFrontend fe lc =
        do let s = httpSettings fe
           info logger $ msg (val "listening HTTP") ~~ "host" .= show (getHost s)
                                                    ~~ "port" .= show (getPort s)
           runSettings s $ gzip def $ jsonp configServer
      where
        logger = clone (Just "http") $ getLogger lc
        configServer req respond =
            do (dims, tree) <- readConfigState lc
               let eContext = queryStringToContext dims query
                   query    = queryString    req
                   method   = requestMethod  req
                   rawPath  = rawPathInfo    req
                   rawQuery = rawQueryString req
               info logger $ "method" .= method ~~ "path" .= rawPath ~~ "query" .= rawQuery
               case eContext of
                 Left e        -> respond $ errorResponse status400 e
                 Right context -> do let Delta cfgData = projection context tree
                                         eFocus        = getSubstructure path cfgData
                                         path          = pathInfo req
                                     case eFocus of
                                       Left e      -> respond $ errorResponse status404 e
                                       Right focus -> respond $ resultResponse focus

jsonHeaders :: ResponseHeaders
jsonHeaders = [(hContentType, "application/json; charset=utf-8")]

errorResponse :: Status -> String -> Response
errorResponse status e   = let jsonResponse     = Object $ H.fromList [("status", jsonStatusCode  )
                                                                      ,("error" , jsonErrorMessage)]
                               jsonStatusCode   = Number $ scientific (toInteger $ statusCode status) 0
                               jsonErrorMessage = String $ T.pack e
                           in  responseLBS status jsonHeaders $ encode jsonResponse

resultResponse :: Value -> Response
resultResponse result = let jsonResponse = Object $ H.fromList [("status", Number 200)
                                                               ,("result", result    )]
                        in responseLBS status200 jsonHeaders $ encode jsonResponse

queryStringToContext :: Dimensions -> Query -> Either String Context
queryStringToContext dims query = let toObjectPair (k,v) = (decodeUtf8 k, String $ decodeUtf8 $ fromJust v)
                                      query'             = map toObjectPair $ filter (isJust . snd) query
                                      ctx                = Object $ H.fromList query'
                                  in  parseEither (parseContext Forgiving dims) ctx
