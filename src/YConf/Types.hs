{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : YConf.Types
Description : Common types, classes and instances
Copyright   : Copyright 2017, Yahoo Holdings Inc.
License     : Licensed under the terms of the BSD3 license. See LICENSE file for terms.
Maintainer  : rodrigosetti@gmail.com
Stability   : experimental
Portability : POSIX
-}
module YConf.Types where

import           Control.Monad
import           Data.Aeson          hiding (encode)
import qualified Data.HashMap.Strict as H
import qualified Data.Text           as T
import qualified Data.Vector         as V
import           Data.Yaml

-- | The raw config is a dimension and collection of rules
data ConfigurationResponse = ConfigResponse Dimensions [Rule] -- ^new or updated configuration
                           | ConfigNotModified                -- ^configuration is not modified

newtype Dimensions = Dimensions [Dimension]

-- | A dimension has a name associated with a mapping of values to their
--   inheritance address
data Dimension = Dimension DimensionName (H.HashMap DimensionValueName DimensionValueAddress)

-- | Delta is a JSON Value associated with a context
data Rule                  = Rule Context Delta

newtype Delta              = Delta Value

type DimensionName         = T.Text

type DimensionValueName    = T.Text

type DimensionValueAddress = [Int]

-- | Ordered list of dimension values as index addresses in the dimension tree
newtype Context = Context [DimensionValueAddress]

instance Monoid Context where

    mempty = Context []

    mappend (Context a) (Context b) =
        Context $ go a b
      where
        go (x:xs) (y:ys) = (if length x > length y then x else y) : go xs ys
        go []    []      = []
        go _     _       = error "context have different sizes"

    mconcat [] = mempty
    mconcat xs = foldr1 mappend xs

instance FromJSON Dimensions where

    parseJSON =
        withArray "dimensions" $ \a ->
            do when (V.null a) $ fail "no \"dimensions\" found"
               case V.head a of
                 Object o -> Dimensions <$> (parseJSON =<< o .: "dimensions")
                 v        -> fail $ "invalid value for \"dimensions\": " ++ show (encode v)

instance FromJSON Dimension where

    parseJSON = withObject "dimension" $ \o ->
        do when (H.size o /= 1) $ fail "dimension should be a singleton object"
           let (name, d) = head $ H.toList o
           case d of
              o'@(Object _) -> (Dimension name . H.fromList) <$> parseDimensionValue [] o'
              x             -> fail $ "invalid dimension value: " ++ show (encode x)
      where
        parseDimensionValue :: DimensionValueAddress -> Value -> Parser [(DimensionValueName, DimensionValueAddress)]
        parseDimensionValue context (Object obj) = let parseWithSubcontext (k, v) n = ((k, subctx):) <$> parseDimensionValue subctx v
                                                                                      where subctx = context ++ [n]
                                                   in  concat <$> zipWithM parseWithSubcontext (H.toList obj) [0..]
        parseDimensionValue _       _            = return []

instance Monoid Delta where

    mempty = Delta $ Object H.empty

    mappend (Delta x) (Delta y) = Delta $ merge x y

-- | Merge two configuration values, giving preference to the second
merge :: Value -> Value -> Value
merge v@(Object o) (Object o')
    | H.null o' = v
    | otherwise = let toInsert = H.toList o'
                  in  Object $ foldr1 (.) (map (uncurry $ H.insertWith $ flip merge) toInsert) o
merge _ x = x

-- | The context parsing mode determines if @parseContext@ will fail with
--   recoverable errors or not.
data ContextParsingMode = Strict | Forgiving

-- | Parser for Context
parseContext :: ContextParsingMode -> Dimensions -> Value -> Parser Context
parseContext _ (Dimensions dims) (String "master") = return $ Context $ replicate (length dims) []
parseContext m dimensions        (String ctxStr)   = case T.breakOn ":" ctxStr of
                                                      (_   , "" )  -> fail $ "could not parse dimension name:value from " ++ show ctxStr
                                                      (name, val)  -> parseContext m dimensions $ Object $ H.singleton name $ String $ T.tail val
parseContext m dimensions        (Array  a)        = mconcat <$> mapM (parseContext m dimensions) (V.toList a)
parseContext mode (Dimensions dims) v =
  withObject "context" parseContextObject v
 where
  parseContextObject o =
    Context <$> forM dims (\(Dimension name values) ->
        do mVal <- o .:? name
           case mVal of
             Nothing           -> return []
             Just (String val) -> let failOrRecover = case mode of
                                                       Strict    -> fail $ "invalid dimension value for " ++ show name ++ ": " ++ show val
                                                       Forgiving -> return []
                                  in  maybe failOrRecover return $ H.lookup val values
             Just _            -> case mode of
                                   Strict    -> fail "dimension value should be a string"
                                   Forgiving -> return [])
