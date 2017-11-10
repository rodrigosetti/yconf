{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : YConf.Live
Description : Exports a type and combinators for working with live reload
Copyright   : Copyright 2017, Yahoo Holdings Inc.
License     : Licensed under the terms of the BSD3 license. See LICENSE file for terms.
Maintainer  : rodrigosetti@gmail.com
Stability   : experimental
Portability : POSIX
-}
module YConf.Live ( LiveConfig(getLogger)
                  , withConfig
                  , readConfigState) where

import           Control.Concurrent
import           Control.Exception.Base
import           Data.IORef
import           Data.Time.Units
import           System.Logger
import           YConf.Backend
import           YConf.Tree
import           YConf.Types

-- | The live config type is where the configuration tree must be fetched from
data LiveConfig = LiveConfig { getLogger :: Logger
                             , getState  :: IORef (Dimensions, ConfigTree) }

-- | Read the configuration state from the live config. The configuration state
--   is composed of the dimensions (for parsing contexts) and the configuration
--   tree (for running the projection algorithm)
readConfigState :: LiveConfig -> IO (Dimensions, ConfigTree)
readConfigState = readIORef. getState

-- | Wrapper for actions that acts on configuration. This is a control combinator
--   that takes a Backend, and runs an action with the live config
withConfig :: Backend a => Logger -> a -> (LiveConfig -> IO b) -> IO b
withConfig logger be action =
    do (be', response) <- loadConfig be
       case response of
        ConfigNotModified                -> fail "initial configuration failed to load."
        ConfigResponse dims rules -> do let tree = buildTree rules
                                        liveConfig <- LiveConfig logger <$> newIORef (dims, tree)
                                        let runAction = action liveConfig
                                        if reloadCheckPoolingInterval be' > 0
                                          then bracket (forkIO $ liveConfigWorker be' liveConfig)
                                                       killThread
                                                       (const runAction)
                                          else runAction
  where
    -- | Live reloader logger
    lcLogger = clone (Just "live") logger

    -- | Infinite action that will keep pooling the Backend and update the live
    --   config state when necessary
    liveConfigWorker b lc =
        do threadDelay $ fromInteger $ toMicroseconds $ reloadCheckPoolingInterval b
           (b', configResponse) <- catch (loadConfig b)
                                    $ \(e :: SomeException) ->
                                         do err lcLogger $ msg (val "error loading configuration") ~~ "exception" .= show e
                                            return (b, ConfigNotModified)
           case configResponse of
            ConfigNotModified         -> debug lcLogger $ msg $ val "configuration is not modified."
            ConfigResponse dims rules -> do let tree = buildTree rules
                                            atomicWriteIORef (getState lc) (dims, tree)
                                            info lcLogger $ msg $ val "configuration was updated."
           liveConfigWorker b' lc
