{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Tests
Description : YConf Tests
Copyright   : Copyright 2017, Yahoo Holdings Inc.
License     : Licensed under the terms of the BSD3 license. See LICENSE file for terms.
Maintainer  : rodrigosetti@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Tests (tests) where

import           Data.Yaml
import           Distribution.TestSuite
import           Helpers
import           YConf.Backend
import           YConf.Backend.FileSystem
import           YConf.Tree
import           YConf.Types

tests :: IO [Test]
tests = return
     [ mkTest "reading context encoded as a array of map" $
           do (dims, _rules) <- loadTestConfig1

              Context ctx1 <- readContext dims "{ \"runtime\": \"client\", \"device\": \"iphone\"}"

              case readContextEither dims "[{ \"runtime\": \"client\"}, {\"device\": \"iphone\"}]" of
                Left err             -> return $ Finished $ Fail err
                Right (Context ctx2) -> return $ if ctx1 == ctx2
                                          then Finished Pass
                                          else Finished $ Fail $ "unexpected context: " ++ show ctx2

     , mkTest "reading context encoded as a array of strings with :" $
           do (dims, _rules) <- loadTestConfig1

              Context ctx1 <- readContext dims "{ \"runtime\": \"client\", \"device\": \"iphone\"}"

              case readContextEither dims "[\"runtime:client\", \"device:iphone\"]" of
                Left err             -> return $ Finished $ Fail err
                Right (Context ctx2) -> return $ if ctx1 == ctx2
                                          then Finished Pass
                                          else Finished $ Fail $ "unexpected context: " ++ show ctx2

     , mkTest "array with a single \"master\" string is master context" $
           do (dims, _rules) <- loadTestConfig1

              case readContextEither dims "[\"master\"]" of
                Left err            -> return $ Finished $ Fail err
                Right (Context ctx) -> return $ if ctx == [[], [], [], []]
                                         then Finished Pass
                                         else Finished $ Fail $ "unexpected context: " ++ show ctx


     , mkTest "no context dimensions should get master configuration" $
           do (dims, rules) <- loadTestConfig1
              let tree = buildTree rules

              ctx <- readContext dims "{}"

              let Delta res = projection ctx tree

              verify res $ \(Object r) -> do locationBackend <- r .: "location_backend"
                                             String hostname <- locationBackend .: "hostname"
                                             assertEquals "geo.data-example.com" hostname

                                             features <- r .: "features"
                                             Bool useLibraryX       <- features .: "use_library_x"
                                             Bool useLibraryY       <- features .: "use_library_y"
                                             Number libraryZVersion <- features .: "library_z_version"

                                             assertEquals True  useLibraryX
                                             assertEquals False useLibraryY
                                             assertEquals 7     libraryZVersion

     , mkTest "context with one dimensional variable" $
           do (dims, rules) <- loadTestConfig1
              let tree = buildTree rules

              ctx <- readContext dims "{ \"runtime\": \"client\"}"

              let Delta res = projection ctx tree

              verify res $ \(Object r) -> do locationBackend <- r .: "location_backend"
                                             String hostname <- locationBackend .: "hostname"

                                             -- Assert that it inherit stuff from master
                                             assertEquals "geo.data-example.com" hostname

                                             -- Assert that it overwrites only the use_library_x
                                             features <- r .: "features"
                                             Bool useLibraryX       <- features .: "use_library_x"
                                             Bool useLibraryY       <- features .: "use_library_y"
                                             Number libraryZVersion <- features .: "library_z_version"

                                             assertEquals False  useLibraryX
                                             assertEquals False useLibraryY
                                             assertEquals 7     libraryZVersion


     , mkTest "context with two dimensional variables" $
           do (dims, rules) <- loadTestConfig1
              let tree = buildTree rules

              ctx <- readContext dims "{ \"runtime\": \"client\", \"device\": \"iphone\"}"

              let Delta res = projection ctx tree

              verify res $ \(Object r) -> do locationBackend <- r .: "location_backend"
                                             String hostname <- locationBackend .: "hostname"

                                             -- Assert that it inherit stuff from master
                                             assertEquals "geo.data-example.com" hostname

                                             -- Assert that it overwrites the use_library_x (client)
                                             -- and library_z_version (iphone)
                                             features <- r .: "features"
                                             Bool useLibraryX       <- features .: "use_library_x"
                                             Bool useLibraryY       <- features .: "use_library_y"
                                             Number libraryZVersion <- features .: "library_z_version"

                                             assertEquals False  useLibraryX
                                             assertEquals False useLibraryY
                                             assertEquals 9     libraryZVersion


     , mkTest "context with specialized variable" $
           do (dims, rules) <- loadTestConfig1
              let tree = buildTree rules

              ctx <- readContext dims "{ \"environment\": \"dev\" }"

              let Delta res = projection ctx tree

              verify res $ \(Object r) -> do locationBackend <- r .: "location_backend"
                                             String hostname <- locationBackend .: "hostname"

                                             -- Assert that it inherit stuff from master
                                             assertEquals "dev.geo.data-example.com" hostname

                                             -- Just make sure other stuff are there
                                             features <- r .: "features"
                                             Bool useLibraryX <- features .: "use_library_x"

                                             assertEquals True useLibraryX

     , mkTest "context with specialized variable but fallback to more general" $
           do (dims, rules) <- loadTestConfig1
              let tree = buildTree rules

              ctx <- readContext dims "{ \"environment\": \"test\" }"

              let Delta res = projection ctx tree

              verify res $ \(Object r) -> do locationBackend <- r .: "location_backend"
                                             String hostname <- locationBackend .: "hostname"

                                             -- Assert that it inherit stuff from master
                                             assertEquals "mock.geo.data-example.com" hostname

                                             -- Just make sure other stuff are there
                                             features <- r .: "features"
                                             Bool useLibraryX <- features .: "use_library_x"

                                             assertEquals True useLibraryX

     , mkTest "one more nesting level (master)" $
           do (dims, rules) <- loadTestConfig3
              let tree = buildTree rules

              ctx <- readContext dims "{}"

              let Delta res = projection ctx tree

              verify res $ \(Object r) -> do yui <- r .: "yui"
                                             Object config <- yui .: "config"
                                             String logLevel <- config .: "logLevel"

                                             assertEquals "info" logLevel

     , mkTest "one more nesting level (merge with development)" $
           do (dims, rules) <- loadTestConfig3
              let tree = buildTree rules

              ctx <- readContext dims "{\"environment\": \"development\"}"

              let Delta res = projection ctx tree

              verify res $ \(Object r) -> do yui <- r .: "yui"
                                             Object config <- yui .: "config"
                                             String logLevel <- config .: "logLevel"

                                             assertEquals "debug" logLevel
  ]

loadTestConfig1 :: IO (Dimensions, [Rule])
loadTestConfig1 = loadTestConfig "./examples/1/"

loadTestConfig3 :: IO (Dimensions, [Rule])
loadTestConfig3 = loadTestConfig "./examples/3/"

loadTestConfig :: FilePath -> IO (Dimensions, [Rule])
loadTestConfig path = do (_, response) <- loadConfig $ fileSystem path
                         case response of
                          ConfigNotModified -> fail "initial configuration failed to load"
                          ConfigResponse dims rules -> return (dims, rules)
