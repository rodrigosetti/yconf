{-|
Module      : YConf.Backend
Description : The Backend class
Copyright   : Copyright 2017, Yahoo Holdings Inc.
License     : Licensed under the terms of the BSD3 license. See LICENSE file for terms.
Maintainer  : rodrigosetti@gmail.com
Stability   : experimental
Portability : POSIX
-}
module YConf.Backend (Backend(..)) where

import YConf.Types
import Data.Time.Units

-- | A backend is something capable of loading a raw configuration, as well
--   as telling if it needs to be updated
class Backend a where

    -- | Load the raw configuration from the backend.
    --   Return the (possible modified) backend and a raw configuration.
    loadConfig                 :: a -> IO (a, ConfigurationResponse)

    -- | Frequency interval (in seconds) to call "loadConfig"
    reloadCheckPoolingInterval :: a -> Second
