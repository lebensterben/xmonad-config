----------------------------------------------------------------------------------------------------
-- |
-- Module      : Main
-- Copyright   : (c) Lucius Hu, 2021
-- License     : BSD3
-- Maintainer  : Lucius Hu <lebensterben@users.noreply.github.com>
--
-- Contains the entry point for XMonad process.
-- Based on the xmonad configuration of Derek Taylor (DistroTube)
-- http://www.gitlab.com/dwt1/
----------------------------------------------------------------------------------------------------

module Main where

import           Custom.Hooks.HandleEventHook             ( myHandleEventHook )
import           Custom.Hooks.LayoutHook                  ( myLayoutHook )
import           Custom.Hooks.LogHook                     ( myLogHook )
import           Custom.Hooks.ManageHook                  ( myManageHook )
import           Custom.Hooks.StartupHook                 ( myStartupHook )
import           Custom.Keymap                            ( addNamedKeys )
import           Custom.Keymap.MajorKeymap                ( myMajorKeymap )
import           Custom.MouseBindings                     ( myMouseBindings )
import           Custom.Util.DBus                         ( mkDbusClient )
import           Custom.Util.Polybar                      ( polybar )
import           Custom.Variables
import           XMonad
import qualified XMonad.Actions.Navigation2D             as Nav2D
import           XMonad.Actions.Navigation2D              ( Navigation2DConfig(..)
                                                          , centerNavigation
                                                          , hybridOf
                                                          , lineNavigation
                                                          , sideNavigation
                                                          , withNavigation2DConfig
                                                          )
import           XMonad.Hooks.EwmhDesktops                ( ewmh )

----------------------------------------------------------------------------------------------------
-- Entry point
----------------------------------------------------------------------------------------------------

-- TODO: unfinished
-- | The config for XMonad, which contains the following parts:
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
main :: IO ()
main = do
    dbus <- mkDbusClient
    xmonad
        $ polybar dbus myStatusBarPP
        . addNamedKeys myMajorKeymap
        . withNavigation2DConfig Nav2D.def
              { defaultTiledNavigation = hybridOf sideNavigation centerNavigation
              , floatNavigation        = hybridOf lineNavigation centerNavigation
              }
        . ewmh
        $ def { modMask         = mod4Mask
              , terminal        = "alacritty"
              , borderWidth     = myBorderWidth
              , workspaces      = myWorkspaces
              , logHook         = myLogHook
              , layoutHook      = myLayoutHook
              , handleEventHook = myHandleEventHook
              , manageHook      = myManageHook myWorkspaces
              , startupHook     = myStartupHook
              , mouseBindings   = myMouseBindings
              }
