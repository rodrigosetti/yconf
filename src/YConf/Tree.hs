{-|
Module      : YConf.Tree
Description : The functions that build and manipulate the configuration tree
Copyright   : Copyright 2017, Yahoo Holdings Inc.
License     : Licensed under the terms of the BSD3 license. See LICENSE file for terms.
Maintainer  : rodrigosetti@gmail.com
Stability   : experimental
Portability : POSIX

Here is the core of the Configuration System. It contains the function to build
the configuration tree from a list of raw rules, and the function to lookup
the configuration projection in that tree given the context.
-}
module YConf.Tree ( ConfigTree(..)
                  , buildTree
                  , projection ) where

import qualified Data.HashMap.Strict as H
import           Data.List           (inits)
import           Data.Maybe
import           Data.Monoid
import           YConf.Types

-- | The Configuration Tree is the object that we keep in memory for quick
--   access for the Projection algorithm. Each node in the tree contains a
--   configuration Value, that gets merged successfully by the projection
--   algorithm as it traverses the tree.
--   The tree edges are context value addresses
data ConfigTree = Node (H.HashMap DimensionValueAddress ConfigTree)
                | Leaf Delta

-- | Build the configuration tree from a raw configuration
buildTree :: [Rule] -> ConfigTree
buildTree rules = foldr1 (.) (map insert rules) empty

empty :: ConfigTree
empty = Node H.empty

-- | Insert delta in the tree, merging the value if there's one with the same
--   context already in the tree
insert :: Rule -> ConfigTree -> ConfigTree
insert (Rule (Context ctx) delta) =
    insert' ctx delta
  where
    insert' []       d           (Leaf val')     = Leaf $ d <> val'
    insert' (a:as)   d           (Node children) = let t = if null as then Leaf d else insert' as d empty
                                                   in  Node $ H.insertWith (const $ insert' as d) a t children
    insert' []       _           (Node _)        = error "YConf.Tree.insert: tree is too deep"
    insert' (_:_)    _           (Leaf _)        = error "YConf.Tree.insert: tree is too shallow"

-- | The Projection Algorithm.
--   Here is the core of the configuration system. Given a context, and a
--   configuration tree, return the configuration value.
projection :: Context -> ConfigTree -> Delta
projection (Context [])           (Leaf d)        = d
projection (Context (dv:subctx))  (Node children) = let visits = inits dv
                                                        values = map (projection $ Context subctx) $ mapMaybe (`H.lookup` children) visits
                                                    in  mconcat values
projection (Context [])           (Node _)        = error "YConf.Tree.projection: config tree too deep"
projection (Context (_:_))        (Leaf _)        = error "YConf.Tree.projection: config tree too shallow"
