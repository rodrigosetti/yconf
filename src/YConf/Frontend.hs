{-|
Module      : YConf.Frontend
Description : The Frontend class
Copyright   : Copyright 2017, Yahoo Holdings Inc.
License     : Licensed under the terms of the BSD3 license. See LICENSE file for terms.
Maintainer  : rodrigosetti@gmail.com
Stability   : experimental
Portability : POSIX
-}
module YConf.Frontend (Frontend(..)) where

import YConf.Live

-- | A Frontend is something capable of listening to "queries" and responding
--   with an encoding of the configuration
class Frontend a where

    -- | Runs the frontend for an undetermined time
    runFrontend :: a -> LiveConfig -> IO ()
