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
    ) where

import           Custom.Workspaces                        ( wsFind )
import           XMonad                                   ( (<||>)
                                                          , (-->)
                                                          , (=?)
                                                          , className
                                                          , composeAll
                                                          , doFloat
                                                          , doShift
                                                          , liftX
                                                          , title
                                                          , XConfig(manageHook)
                                                          )
import           XMonad.Hooks.ManageHelpers               ( (-->>)
                                                          , (</=?)
                                                          , composeOne
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

-- | Actions to run when a new window is opened.
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
                           [ title =? "Mozilla Firefox" -?> shiftIfFoundWS "www"
                           -- , className =? "mpv" --> doShift (workspaces !! 7)
                           , className =? "Hexchat" -?> shiftIfFoundWS "chat"
                           , className =? "Gitter" -?> shiftIfFoundWS "chat"
                           , className =? "Element" -?> shiftIfFoundWS "chat"
                           , className =? "vlc" -?> shiftIfFoundWS "vid"
                           , className =? "Gimp" -?> shiftIfFoundWS "gfx" >> doFloat
                           , className =? "Xmessage" <||> className =? "Gxmessage" -?> doCenterFloat
                           , className =? "Ulauncher" -?> doCenterFloat
                           -- FIXME: I don't have virtual box
                           -- , title =? "Oracle VM VirtualBox Manager" --> doFloat
                           -- , className =? "VirtualBox Manager" --> doShift (workspaces !! 4)
                           ]
                       -- FIXME: I don't currently use ScratchPads
                       -- ,namedScratchpadManageHook myScratchPads
                       ]
    }
    where shiftIfFoundWS x = liftX (wsFind x) </=? Nothing -->> \ ~(Just a) -> doShift a
