{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : YConf.Backend.Helpers
Description : Helper functions useful for all backends
Copyright   : Copyright 2017, Yahoo Holdings Inc.
License     : Licensed under the terms of the BSD3 license. See LICENSE file for terms.
Maintainer  : rodrigosetti@gmail.com
Stability   : experimental
Portability : POSIX
-}
module YConf.Backend.Helpers where

import           Data.Aeson
import qualified Data.ByteString     as BS
import qualified Data.HashMap.Strict as H
import           Data.List
import qualified Data.Vector         as V
import           Data.Yaml
import           YConf.Types

-- | Given the parsed dimensions, and the data for the rules, return the
--   parsed rules
parseRules :: Dimensions -> [BS.ByteString] -> Either ParseException [Rule]
parseRules dims bss =
    (concat <$>) . mapM (parseMonad $ parseRule dims) =<< mapM decodeEither' bss

parseRule :: Dimensions -> Value -> Parser [Rule]
parseRule dims =
    withArray "rules" $ \arr -> mapM parseSingle $ V.toList arr
  where
    parseSingle = withObject "rule" $ \o ->
        do context <- o .:? "settings" .!= Object H.empty
           encoded <- parseContext Strict dims context
           return $ Rule encoded $ Delta $ Object $ H.delete "settings" o

-- | Helper list processing function that finds an element in the list that
--   matches the predicate, and returns a tuple with that element and the
--   list without that element
findApart :: Eq a => (a -> Bool) -> [a] -> Maybe (a, [a])
findApart p l = let apart x = (x, delete x l)
                in  apart <$> find p l

printParseException :: ParseException -> String
printParseException NonScalarKey                           = "key is not a scalar value"
printParseException (UnknownAlias a)                       = "alias anchor name unknown: " ++ a
printParseException (UnexpectedEvent s e)                  = "unexpected event: " ++ show s ++ ", expected: " ++ show e
printParseException (InvalidYaml Nothing)                  = "invalid YAML"
printParseException (InvalidYaml (Just (YamlException e))) = "invalid YAML: " ++ e
printParseException (InvalidYaml (Just (YamlParseException p c (YamlMark idx lin col)))) =
    "invalid YAML: " ++ p ++ " " ++ c ++ ", line: " ++ show lin ++ ", column: " ++ show col ++ ", index: " ++ show idx
printParseException (AesonException e)      = "Parsing exception: " ++ e
printParseException (OtherParseException e) = "Parsing exception: " ++ show e
printParseException e = "Exception: " ++ show e
