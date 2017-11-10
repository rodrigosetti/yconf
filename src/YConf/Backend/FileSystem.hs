{-|
Module      : YConf.Backend.FileSystem
Description : The implementation for the file system backend
Copyright   : Copyright 2017, Yahoo Holdings Inc.
License     : Licensed under the terms of the BSD3 license. See LICENSE file for terms.
Maintainer  : rodrigosetti@gmail.com
Stability   : experimental
Portability : POSIX
-}
module YConf.Backend.FileSystem ( FileSystem
                                , fileSystem) where

import           Control.Concurrent.Async
import qualified Data.ByteString          as BS
import           Data.List
import           Data.Time.Clock          (UTCTime)
import           Data.Yaml
import           System.Directory
import           System.FilePath.Posix
import           YConf.Backend
import           YConf.Backend.Helpers
import           YConf.Types

-- | Internal representation of the state of the FileSystem Backend
data FileSystem = FileSystem { lastModification :: Maybe UTCTime
                             , root             :: FilePath }

-- | Public constructor for a FileSystem Backend
fileSystem :: FilePath -> FileSystem
fileSystem = FileSystem Nothing

-- | The Backend instance implementation for FileSystem
instance Backend FileSystem where

    -- | Read all JSON and YAML files in the root folder, parses it into a
    --   "RawConfig" value, returned along with the modified state of
    --   file-system
    loadConfig fs@FileSystem { root             = dir
                             , lastModification = mt } =
     do lastModified <- maximum <$> (getConfigFiles dir >>= mapConcurrently getModificationTime)
        let fs' = fs { lastModification = Just lastModified }
        if maybe False (>= lastModified) mt
        then return (fs', ConfigNotModified)
        else do configFiles <- getConfigFiles dir
                case findApart ("dimensions." `isInfixOf`) configFiles of
                   Nothing                                   -> fail $ "could not find dimensions file in " ++ dir
                   Just (dimensionsFileName, deltaFileNames) ->
                      do eDimensions <- decodeEither' <$> BS.readFile dimensionsFileName
                         case eDimensions of
                           Left err   -> fail $ "parsing error in " ++ dimensionsFileName ++ ": " ++ printParseException err
                           Right dims ->
                              do rules <- parseRules dims <$> mapConcurrently BS.readFile deltaFileNames
                                 case ConfigResponse dims <$> rules of
                                     Right config -> return (fs', config)
                                     Left  err    -> fail $ "parsing error: " ++ printParseException err

    -- | file system backend check for modifications every second. It's not
    --   that expensive to read the files last modification date
    reloadCheckPoolingInterval _ = 1

-- | Given the root path, return all the file paths for files that matches the
--   configuration file name pattern
getConfigFiles :: FilePath -> IO [FilePath]
getConfigFiles r =
    map prependDir . filter isConfigFile <$> getDirectoryContents r
  where
    prependDir p = joinPath [r, p]
    isConfigFile = let validSuffixes = [".json", ".yaml", ".yml"]
                    in or . flip map (map isSuffixOf validSuffixes) . flip id
