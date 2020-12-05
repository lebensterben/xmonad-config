----------------------------------------------------------------------------------------------------
-- |
-- Module      : Custom.Hooks.ManageHook
-- Copyright   : (c) Lucius Hu, 2020
-- License     : BSD3
-- Maintainer  : Lucius Hu <lebensterben@users.noreply.github.com>
--
-- Defines actions to run when a new window is opened.
----------------------------------------------------------------------------------------------------

module Custom.Hooks.ManageHook
    (
    -- * Manage Hook
      myManageHook
    )
where

import           Custom.Variables                         ( myWorkspaces )
import           Custom.Workspaces                        ( wsLabels )
import           XMonad                                   ( XConfig(manageHook)
                                                          , (-->)
                                                          , (=?)
                                                          , className
                                                          , doFloat
                                                          , doShift
                                                          , title
                                                          )
import           XMonad.ManageHook                        ( composeAll )
import           XMonad.Hooks.ManageHelpers               ( composeOne
                                                          , doCenterFloat
                                                          , doFullFloat
                                                          , isDialog
                                                          , isFullscreen
                                                          , transience'
                                                          , (-?>)
                                                          )

----------------------------------------------------------------------------------------------------
-- Mangage Hook
----------------------------------------------------------------------------------------------------

-- | TODO
-- Actions to run when a new window is opened.
--
-- * If the new window wants to fill the fullscreen, makes it a float window
-- * If the new window is a dialog, make it a centered float window
-- * If the new window is transient, sends it to its parent window
-- * If certain 'XMonad.Query' returns true, sent the window to a given workspace.
myManageHook :: XConfig l -> XConfig l
myManageHook conf = conf
    { manageHook = composeAll
                       [ isFullscreen --> doFullFloat
                       , isDialog --> doCenterFloat -- Float Dialog
                       , transience' -- send transient window to its parent
                       , composeOne -- send windows to designated ws
                          -- Note: using @doShift ( workspaces !! i)@ sends program to workspace @i + 1@
                           [ title =? "Mozilla Firefox" -?> doShift (wsLabels myWorkspaces !! 1)
                           -- , className =? "mpv" --> doShift (workspaces !! 7)
                           , className =? "Hexchat" -?> doShift (wsLabels myWorkspaces !! 5)
                           , className =? "Gitter" -?> doShift (wsLabels myWorkspaces !! 5)
                           , className =? "Element" -?> doShift (wsLabels myWorkspaces !! 5)
                           , className =? "vlc" -?> doShift (wsLabels myWorkspaces !! 7)
                           , className =? "Gimp" -?> doShift (wsLabels myWorkspaces !! 8) >> doFloat
                           -- FIXME: I don't have virtual box
                           -- , title =? "Oracle VM VirtualBox Manager" --> doFloat
                           -- , className =? "VirtualBox Manager" --> doShift (workspaces !! 4)
                           ]
                       -- FIXME: I don't currently use ScratchPads
                       -- ,namedScratchpadManageHook myScratchPads
                       ]
    }
