----------------------------------------------------------------------------------------------------
-- |
-- Module      : Custom.Util.Apps
-- Copyright   : (c) Lucius Hu, 2020
-- License     : BSD3
-- Maintainer  : Lucius Hu <lebensterben@users.noreply.github.com>
--
-- Utilities for managing default apps, launcher apps, etc.
----------------------------------------------------------------------------------------------------

module Custom.Util.Apps where

import           Data.Default                             ( Default(..) )

data App = App {
    executable :: String,  -- ^ Name of the program, or full path to the executable.
    description :: String, -- ^ Description.
    evalFlag :: String,    -- ^ Flag used for evaluating extra commands.
    evalWrapper :: String -> String -> [String] -> String
                           -- ^ A function that takes executable, evaluation flag, and a list of
                           --   arguments, and returns runnable command.
               }

instance Default App where
    def =
        App { executable = undefined, description = "", evalFlag = "", evalWrapper = \_ _ _ -> "" }

data DefaultApps = DefaultApps
    {myTerminal, myBrowser, myEditor :: [String]}
