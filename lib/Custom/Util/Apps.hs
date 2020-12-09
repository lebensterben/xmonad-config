{-# LANGUAGE DeriveDataTypeable #-}
----------------------------------------------------------------------------------------------------
-- |
-- Module      : Custom.Util.Apps
-- Copyright   : (c) Lucius Hu, 2020
-- License     : BSD3
-- Maintainer  : Lucius Hu <lebensterben@users.noreply.github.com>
--
-- Utilities for managing default apps, and to launcher apps, etc.
----------------------------------------------------------------------------------------------------

module Custom.Util.Apps where

import           XMonad                                   ( ExtensionClass(..)
                                                          , Typeable
                                                          , MonadIO
                                                          )
import qualified XMonad.Util.ExtensibleState             as XS
import           XMonad.Util.PureX                        ( XLike )
import           XMonad.Util.Run                          ( safeSpawn )

class Runnable r where
    appRun :: (MonadIO m, XLike m) => r -> m ()
    appDscr :: r -> (String, String)

class Runnable r => RunnableArgs r where
    appRunArgs :: (MonadIO m, XLike m) => r -> [Arg] -> m()

newtype DefaultAppsStorage = DefaultAppsStorage DefaultApps deriving Typeable
instance ExtensionClass DefaultAppsStorage where
    initialValue = DefaultAppsStorage DefaultApps { termEmulator = App "" "" "" id
                                                  , browser      = App "" "" "" id
                                                  , fileManager  = App "" "" "" id
                                                  , editor       = App "" "" "" id
                                                  }

data DefaultApps = DefaultApps
    {termEmulator, browser, fileManager, editor :: App}

type Executable = FilePath
type Arg = String
type ArgsWrapper = [Arg] -> [Arg]
type URL = String

data App = App
    Executable  -- ^ Name of the program.
    String      -- ^ Shoft Description.
    String      -- ^ Long Description.
    ArgsWrapper -- ^ A function specifying how to formulate arguments and flags supplied to this
                --   program.

-- default flags + space separated evaluation flag-args
simpleArgsWrapper :: [Arg] -> Arg -> ([Arg] -> [Arg])
simpleArgsWrapper dflags eflag args = dflags ++ concat [ [eflag, arg] | arg <- args ]

-- space separated evaluation flag-args
simplerArgsWrapper :: Arg -> ([Arg] -> [Arg])
simplerArgsWrapper = simpleArgsWrapper []

-- simplest
simplestArgsWrapper :: ([Arg] -> [Arg])
simplestArgsWrapper = id

instance Runnable App where
    appRun (App exec _ _ wrapper) = safeSpawn exec $ wrapper []
    appDscr (App _ sdscr ldscr _) = (sdscr, ldscr)

instance RunnableArgs App where
    appRunArgs (App exec _ _ wrapper) args = safeSpawn exec $ wrapper args

data FlatpakApp = FlatpakApp
    String      -- ^ Identifier of the program.
    String      -- ^ Shoft Description.
    String      -- ^ Long Description.

instance Runnable FlatpakApp where
    appRun (FlatpakApp ident _ _) = safeSpawn "flatpak" ["run", ident]
    appDscr (FlatpakApp _ sdscr ldscr) = (sdscr, ldscr)

data TerminalApp = TerminalApp
    Executable  -- ^ Name of the program.
    String      -- ^ Shoft Description.
    String      -- ^ Long Description.
    ArgsWrapper -- ^ A function specifying how to formulate arguments and flags supplied to this
                --   program.

instance Runnable TerminalApp where
    appRun (TerminalApp exec _ _ wrapper) = do
        (DefaultAppsStorage DefaultApps { termEmulator = term }) <- XS.get
        appRunArgs term (exec : wrapper [])

    appDscr (TerminalApp _ sdscr ldscr _) = (sdscr, ldscr)

instance RunnableArgs TerminalApp where
    appRunArgs (TerminalApp exec _ _ wrapper) args = do
        (DefaultAppsStorage DefaultApps { termEmulator = term }) <- XS.get
        appRunArgs term (exec : wrapper args)

data WebApp = WebApp
    URL     -- ^ Name of the program.
    String  -- ^ Shoft Description.
    String  -- ^ Long Description.

instance Runnable WebApp where
    appRun (WebApp url _ _) = do
        (DefaultAppsStorage DefaultApps { browser = brws }) <- XS.get
        appRunArgs brws [url]

    appDscr (WebApp _ sdscr ldscr) = (sdscr, ldscr)
