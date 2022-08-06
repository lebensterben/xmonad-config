----------------------------------------------------------------------------------------------------
-- |
-- Module      : Main
-- Copyright   : (c) Lucius Hu, 2020-2022
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
import           Custom.Hooks.ManageHook                  ( myManageHook )
import           Custom.Hooks.StartupHook                 ( myStartupHook )
import           Custom.Keymap                            ( myMajorKeymap )
import           Custom.MouseBindings                     ( myMouseBindings )
import           Custom.Util.Keymap                       ( addNamedKeys )
import           Custom.Variables                         ( myBorderWidth
                                                          , myWorkspaces
                                                          )
import           XMonad                                   ( (<+>)
                                                          , Default(def)
                                                          , XConfig(..)
                                                          , mod4Mask
                                                          , xmonad
                                                          )
import qualified XMonad.Actions.Navigation2D             as Nav2D
import           XMonad.Actions.Navigation2D              ( Navigation2DConfig(..)
                                                          , centerNavigation
                                                          , hybridOf
                                                          , lineNavigation
                                                          , sideNavigation
                                                          , withNavigation2DConfig
                                                          )
import           XMonad.Actions.SwapPromote               ( masterHistoryHook )
import           XMonad.Hooks.EwmhDesktops                ( ewmh )
import           XMonad.Hooks.ManageDocks                 ( docks )
import           XMonad.Hooks.TaffybarPagerHints          ( pagerHints )
import           XMonad.Hooks.WorkspaceHistory            ( workspaceHistoryHook )

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
-- [@'withNavigation2DConfig'@]:
--
--     [@'XMonad.Core.startupHook'@]: Modifies directional window navigation strategies.
main :: IO ()
main =
    xmonad
        . addNamedKeys myMajorKeymap
        . withNavigation2DConfig Nav2D.def
              { defaultTiledNavigation = hybridOf sideNavigation centerNavigation
              , floatNavigation        = hybridOf lineNavigation centerNavigation
              }
        . docks
        . ewmh
        . pagerHints
        $ def { modMask         = mod4Mask
              , terminal        = "alacritty"
              , borderWidth     = myBorderWidth
              , workspaces      = myWorkspaces
              , logHook         = workspaceHistoryHook <+> masterHistoryHook
              , layoutHook      = myLayoutHook
              , handleEventHook = myHandleEventHook
              , manageHook      = myManageHook
              , startupHook     = myStartupHook
              , mouseBindings   = myMouseBindings
              }
