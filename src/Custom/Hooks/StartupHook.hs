----------------------------------------------------------------------------------------------------
-- |
-- Module      : Custom.Hooks.StartupHook
-- Copyright   : (c) Lucius Hu, 2020-2022
-- License     : BSD3
-- Maintainer  : Lucius Hu <lebensterben@users.noreply.github.com>
--
-- Defines actions to run after xmonad starts up.
----------------------------------------------------------------------------------------------------
module Custom.Hooks.StartupHook (myStartupHook) where

import           Custom.Variables                         ( startUpApps )
import           System.Posix.Env                         ( putEnv )
import           XMonad                                   ( (<+>)
                                                          , X
                                                          , io
                                                          , xC_left_ptr
                                                          )
import           XMonad.Hooks.SetWMName                   ( setWMName )
import           XMonad.Util.Cursor                       ( setDefaultCursor )
import           XMonad.Util.SpawnOnce                    ( spawnOnce )

----------------------------------------------------------------------------------------------------
-- Startup Hook
----------------------------------------------------------------------------------------------------

-- | Actions to run when starting up xmonad.
--
-- - Set \"WM\" (window manager) name to \"LG3D\", and set environment variable \"_JAVA_AWT_WM_NONREPARENTING=1\",
--   which supposedly fix a Java GUI program bug.
-- - Set X Cursor.
-- - Launch external programs. See 'startUpApps'.
myStartupHook :: X ()
myStartupHook =
    (setWMName "LG3D" *> io (putEnv "_JAVA_AWT_WM_NONREPARENTING=1"))
        <+> setDefaultCursor xC_left_ptr
        <+> mapM_ spawnOnce startUpApps
