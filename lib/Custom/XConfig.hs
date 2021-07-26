----------------------------------------------------------------------------------------------------
-- |
-- Module      : Custom.XConfig
-- Copyright   : (c) Lucius Hu, 2020
-- License     : BSD3
-- Maintainer  : Lucius Hu <lebensterben@users.noreply.github.com>
--
-- Defines the complete configuration for XMonad.
----------------------------------------------------------------------------------------------------

module Custom.XConfig (myXConfig) where

import           Custom.Hooks.HandleEventHook             ( myHandleEventHook )
import           Custom.Hooks.LayoutHook                  ( myLayoutHook )
import           Custom.Hooks.LogHook                     ( myLogHook )
import           Custom.Hooks.ManageHook                  ( myManageHook )
import           Custom.Hooks.StartupHook                 ( myStartupHook )
import           Custom.Hooks.WindowNavigationHook        ( windowNavigationHook )
import           Custom.Keymap                            ( installNamedMajorKeys )
import           Custom.Keymap.MajorKeymap                ( myMajorKeymap )
import           Custom.MouseBindings                     ( defineMouseBindings
                                                          , myMouseBindings
                                                          )
import           Custom.Util.Polybar                      ( polybar )
import           Custom.Variables                         ( baseXConfig
                                                          , myStatusBarPP
                                                          )
import           XMonad.Hooks.EwmhDesktops                ( ewmh )

----------------------------------------------------------------------------------------------------
-- XMonad Main Config
----------------------------------------------------------------------------------------------------

-- TODO: unfinished
-- | The config for XMonad, which contains the following parts:
--
-- [@'baseXConfig'@]: Sets up variables from "Custom.Variables" module:
--
--      * 'XMonad.Core.modMask'
--      * 'XMonad.Core.terminal' -- TODO add description?
--      * 'XMonad.Core.borderWidth'
--      * 'XMonad.Core.normalBorderColor'
--      * 'XMonad.Core.focusedBorderColor'
--
-- [@'XMonad.Hooks.EwmhDesktops.ewmh'@]: Adds EWMH functionalities via the following hooks.
--
--     [@'XMonad.Core.startupHook'@]:
--
--         [@'XMonad.Hooks.EwmhDesktops.ewmhDesktopStartup'@]: Starts EwmhDesktops and advertises
--         EWMH support to X server.
--
--     [@'XMonad.Core.handleEventHook'@]:
--
--         [@'XMonad.Hooks.EwmhDesktops.ewmhDesktopsEventHook'@]: Intercepts messages from pagers
--         and similars.
--
--     [@'XMonad.Core.logHook'@]:
--
--         [@'XMonad.Hooks.EwmhDesktops.ewmhDesktopLogHook'@]: Notifies pagers and window lists of
--         the current state of workspaces and windows.
--
-- [@'windowNavigationHook'@]:
--
--     [@'XMonad.Core.startupHook'@]: Modifies directional window navigation strategies.
--     TODO: UNFINISHED
myXConfig dbus =
    polybar dbus myStatusBarPP
        . ewmh
        . windowNavigationHook
        . myStartupHook
        . myManageHook
        . myHandleEventHook
        . myLogHook
        . myLayoutHook
        . defineMouseBindings myMouseBindings
        . flip installNamedMajorKeys myMajorKeymap
        $ baseXConfig
-- handleEventHook
-- Run xmonad commands from command line with "xmonadctl command". Commands include:
-- shrink, expand, next-layout, default-layout, restart-wm, xterm, kill, refresh, run,
-- focus-up, focus-down, swap-up, swap-down, swap-master, sink, quit-wm. You can run
-- "xmonadctl 0" to generate full list of commands written to ~/.xsession-errors.
-- To compile xmonadctl: ghc -dynamic xmonadctl.hs
