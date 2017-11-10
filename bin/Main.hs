{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Main
Description : Application main entry point
Copyright   : Copyright 2017, Yahoo Holdings Inc.
License     : Licensed under the terms of the BSD3 license. See LICENSE file for terms.
Maintainer  : rodrigosetti@gmail.com
Stability   : experimental
Portability : POSIX

Parse command line options and glue together backend and frontend
-}
module Main (main) where

import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Data.Aeson
import           Data.Maybe
import           Data.Semigroup             ((<>))
import qualified Data.Text                  as T
import           Data.Version               (showVersion)
import           Data.Yaml
import           Options.Applicative        as Opt
import           Paths_yconf                (version)
import           System.Exit
import           System.IO
import           System.Logger              as Log
import           YConf.Backend
import           YConf.Backend.FileSystem
import           YConf.Backend.HTTP
import           YConf.Frontend
import           YConf.Frontend.HTTP
import           YConf.Frontend.MessagePack
import           YConf.Live

-- | Program options
data Options = Options { optConfigFile :: Maybe FilePath
                       , optPath       :: Maybe FilePath
                       , optBackendURL :: Maybe String
                       , optHTTP       :: Maybe String
                       , optMsgPackRPC :: Maybe String
                       , optLogOutput  :: Output
                       , optLogLevel   :: Level }

defaultLogOutput :: Output
defaultLogOutput = StdErr

defaultLogLevel :: Level
defaultLogLevel = Info

instance FromJSON Options where
    parseJSON = withObject "options" $ \o ->
                    Options Nothing
                        <$> ( o .:? "path")
                        <*> ( o .:? "url")
                        <*> ( o .:? "http")
                        <*> ( o .:? "msgpack-rpc")
                        <*> ( o .:? "log-output" .!= defaultLogOutput)
                        <*> ( o .:? "log-level"  .!= defaultLogLevel)

instance FromJSON Output where
    parseJSON = withText "log output" $ \t -> return $ Path $ T.unpack t

instance FromJSON Level where
    parseJSON = withText "log level" $ \t -> return $ read $ T.unpack t

-- | How to parse the program options
parseOptions :: Opt.Parser Options
parseOptions = Options <$> option (eitherReader (Right . Just))
                                  ( long "yconf-config"
                                  <> metavar "PATH"
                                  <> value Nothing
                                  <> hidden
                                  <> help "Optional configuration file for yconf (alternative to command line)" )
                       <*> option (eitherReader (Right . Just))
                                  (  long "path"
                                  <> short 'p'
                                  <> metavar "PATH"
                                  <> value Nothing
                                  <> help "location of configuration files" )
                       <*> option (eitherReader (Right . Just))
                                  (  long "url"
                                  <> short 'u'
                                  <> metavar "URL"
                                  <> value Nothing
                                  <> help "URL of configuration master" )
                       <*> option (eitherReader (Right . Just))
                                  (  long "http"
                                  <> metavar "[BIND:]PORT"
                                  <> value Nothing
                                  <> help "bind host and port to start the HTTP server")
                       <*> option (eitherReader (Right . Just))
                                  (  long "rpc"
                                  <> metavar "[BIND:]PORT"
                                  <> value Nothing
                                  <> help "bind host and port to start the TCP message pack RPC server")
                       <*> option (eitherReader (Right . Path))
                                  (  long "log-output"
                                  <> short 'l'
                                  <> metavar "PATH"
                                  <> value defaultLogOutput
                                  <> hidden
                                  <> help "file to write log (default to stderr)")
                       <*> option auto  (  long "log-level"
                                        <> metavar "[Trace|Debug|Info|Warn|Error|Fatal]"
                                        <> value defaultLogLevel
                                        <> showDefault
                                        <> hidden
                                        <> help "log level")

main :: IO ()
main =
    execParser opts >>= reloadFromFile >>= run
  where
    opts = Opt.info (helper <*> parseOptions)
      (  fullDesc
      <> progDesc "YConf is a configuration server. It contains a collection of configuration \
                  \files (JSON or YAML) that specify multi-dimensional configuration data \
                  \(like YCB, but with some minor design differences), and respond to \
                  \contextual queries from clients in either a RESTful HTTP frontend or a \
                  \message pack RPC frontend (or both simultaneously)."
      <> header ("YConf " ++ showVersion version ++ " (c) 2017 Yahoo Holdings Inc."))
    reloadFromFile opt@Options { optConfigFile = Just path } =
        do r <- decodeFileEither path
           case r of
            Left e -> do hPutStrLn stderr $ "error parsing yconf configuration file: " ++ path ++ ": " ++ show e
                         exitFailure >> return opt
            Right cfg -> return cfg
    reloadFromFile opt = return opt
    run options =
      do
         logger <- new $ setOutput (optLogOutput options)
                       $ setName (Just "main")
                       $ setBufSize (if optLogOutput options == StdErr then 64 else 1024)
                       $ setLogLevel (optLogLevel options) defSettings

         when (isNothing (optHTTP options) && isNothing (optMsgPackRPC options)) $ do
             fatal logger $ msg $ val "Specify at least one frontend (see --help)"
             flush logger
             exitFailure

         let duplicatedBackendError = do fatal logger $ msg $ val "Specify only one backend (see --help)"
                                         flush logger
                                         exitFailure
             noBackendError         = do fatal logger $ msg $ val "Specify one backend (see --help)"
                                         flush logger
                                         exitFailure

         let buildRunner :: Backend a => a -> (LiveConfig -> IO b) -> IO b
             buildRunner = withConfig logger

         runner <- case optPath options of
                        Just path -> maybe (return $ buildRunner $ fileSystem path) (const duplicatedBackendError) $ optBackendURL options
                        Nothing   -> maybe noBackendError return $ (buildRunner . http) <$> optBackendURL options

         runner $ \lc ->
             do let startMsgPackRPC = case optMsgPackRPC options of
                                        Just bind -> runFrontend (messagePackFrontend bind) lc
                                        Nothing   -> return ()
                    startHTTP       = case optHTTP options of
                                        Just bind -> runFrontend (httpFrontend bind) lc
                                        Nothing   -> return ()
                bracket (mapM async [startHTTP, startMsgPackRPC])
                        (const $ flush logger)
                        (mapM_ wait)
