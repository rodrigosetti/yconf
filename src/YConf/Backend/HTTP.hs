{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : YConf.Backend.HTTP
Description : The implementation for the HTTP backend
Copyright   : Copyright 2017, Yahoo Holdings Inc.
License     : Licensed under the terms of the BSD3 license. See LICENSE file for terms.
Maintainer  : rodrigosetti@gmail.com
Stability   : experimental
Portability : POSIX

The HTTP Backend keeps pooling (every 10 seconds or so) a configured URL which
hosts a "configuration manifest" data.

The configuration manifest data is simply a YAML content defining three
things:

 * The configuration hash - this is used to keep track whether or not Yconf
   should update the configuration tree.
 * The dimensions URL (relative or absolute).
 * A list of rules URLs (relative or absolute).

Example:

    hash: f84adeee0cbf2a267e00df741d7d3c66
    dimensions: /dimensions.yml
    rules:
        - /config1.yml
        - /config2.json

Whenever the hash changes, the configuration system is reloaded.
-}
module YConf.Backend.HTTP ( HTTP
                          , http ) where

import           Control.Concurrent.Async
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString              as BS
import           Data.Maybe
import qualified Data.Text                    as T
import           Data.Yaml
import           Network.HTTP.Client
import           Network.HTTP.Client.Internal (setUri, setUriRelative)
import           Network.HTTP.Types
import           Network.URI
import           YConf.Backend
import           YConf.Backend.Helpers
import           YConf.Types

-- | The HTTP Backend state type
data HTTP = HTTP { httpStateHash :: Maybe String -- ^ content hash of the entire configuration
                 , manifestURI   :: URI }        -- ^ URL to fetch the "configuration manifest"

-- | Public constructor for an HTTP Backend object
http :: String -> HTTP
http uri = HTTP Nothing $ fromMaybe (error $ "invalid URL: " ++ uri) $ parseURI uri

-- | The configuration manifest object defines where is the dimensions and
--   the rules.
data ConfigurationManifest =
    ConfigurationManifest { stateHash     :: Maybe String  -- ^ content hash of the entire configuration,
                                                           --   if missing, assume content is never updated
                          , dimensionsURL :: URI     -- ^ where the dimensions data is available
                          , rulesURLs     :: [URI] } -- ^ list of urls to fetch rules data from

instance FromJSON ConfigurationManifest where

    parseJSON = withObject "configuration manifest" $ \o ->
                    ConfigurationManifest <$> o .:? "hash"
                                          <*> o .:  "dimensions"
                                          <*> o .:  "rules"

instance FromJSON URI where

    parseJSON = withText "URL" $ \t ->
                   let s = T.unpack t
                   in  maybe (fail $ "invalid URL: " ++ s) return $
                        parseURIReference s

instance Backend HTTP where

    loadConfig h@HTTP { manifestURI   = url
                      , httpStateHash = curHash } =
     do m <- newManager defaultManagerSettings
        indexReq <- setUri defaultRequest url
        state    <- httpDecode m indexReq

        let h' = h { httpStateHash = stateHash state }

        if fromMaybe False $ (==) <$> stateHash state <*> curHash
        then return (h', ConfigNotModified)
        else do dimsReq <- setUriRelative indexReq $ dimensionsURL state
                dims    <- httpDecode m dimsReq
                rules   <- parseRules dims <$> mapConcurrently (setUriRelative indexReq >=> httpRead m) (rulesURLs state)
                case ConfigResponse dims <$> rules of
                    Right config -> return (h', config)
                    Left  err    -> fail $ "parsing error: " ++ printParseException err

    reloadCheckPoolingInterval _ = 10

-- | GET a Request and return the response. Fail if response status is non-200
httpDecode :: FromJSON a => Manager -> Request -> IO a
httpDecode manager req = do body <- httpRead manager req
                            case decodeEither' body of
                             Left err -> fail $ "error parsing " ++ show (getUri req) ++ ": " ++ printParseException err
                             Right r  -> return r

httpRead :: Manager -> Request -> IO BS.ByteString
httpRead manager request =
    withResponse request manager $
      \response -> do let status = responseStatus response
                      when (statusCode status /= 200) $
                          fail $ show (getUri request) ++ " returned a non-200 status code: " ++ show status
                      BS.concat <$> brConsume (responseBody response)
