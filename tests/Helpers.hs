{-|
Module      : Helpers
Description : Testing helpers
Copyright   : Copyright 2017, Yahoo Holdings Inc.
License     : Licensed under the terms of the BSD3 license. See LICENSE file for terms.
Maintainer  : rodrigosetti@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Helpers where

import           Control.Monad
import qualified Data.ByteString        as BS
import           Data.Yaml
import           Distribution.TestSuite
import           YConf.Types

mkTest :: String -> IO Progress -> Test
mkTest n r = let self = TestInstance { run       = r
                                     , name      = n
                                     , tags      = []
                                     , options   = []
                                     , setOption = \_ _ -> Right self }
             in Test self

-- | Parses a context
readContext :: Monad m => Dimensions -> BS.ByteString -> m Context
readContext dims ctx = either fail return $ readContextEither dims ctx

readContextEither :: Dimensions -> BS.ByteString -> Either String Context
readContextEither dims ctx = decodeEither ctx >>= parseEither (parseContext Strict dims)

verify :: Monad m => a -> (a -> Parser b) -> m Progress
verify x = return .Finished . either Fail (const Pass) . flip parseEither x

verifyEither :: (Monad m) => Either String b -> m Progress
verifyEither = return . Finished . either Fail (const Pass)

assertEquals :: (Monad m, Show a, Eq a) => a -> a -> m  ()
assertEquals x y = when (x /= y) $ fail $ "Fail: not equals. Expected: " ++ show x ++ ", observed: " ++ show y
