{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : YConf.Frontend.Helpers
Description : Helpers for the frontend operations (internal)
Copyright   : Copyright 2017, Yahoo Holdings Inc.
License     : Licensed under the terms of the BSD3 license. See LICENSE file for terms.
Maintainer  : rodrigosetti@gmail.com
Stability   : experimental
Portability : POSIX
-}
module YConf.Frontend.Helpers where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Text        as T
import           Data.Vector      ((!?))
import           Text.Read        (readEither)

-- | path is a list of property names
type Path = [T.Text]

-- | Get a JSON substructure from a value defined by a path
getSubstructure :: Path -> Value -> Either String Value
getSubstructure path =
    parseEither (parser path)
 where
    parser []     v          = return v
    parser ("":_) v          = return v
    parser (p:ps) (Object o) = o .: p >>= parser ps
    parser (p:ps) (Array  a) = case readEither $ T.unpack p of
                                Left  _ -> fail $  show p ++ " is not a valid array index"
                                Right n -> maybe (fail $ "index " ++ show n ++ " is out of bounds")
                                                 (parser ps)
                                                 (a !? n)
    parser ps     _          = fail $ "could not access path: " ++ show ps
