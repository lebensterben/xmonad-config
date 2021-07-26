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

import           Custom.Variables                         ( myScratchPads
                                                          , myWorkspaces
                                                          )
import           Data.List                                ( find )
import           XMonad                                   ( (-->)
                                                          , (<||>)
                                                          , (=?)
                                                          , ManageHook
                                                          , Query
                                                          , WorkspaceId
                                                          , XConfig(manageHook)
                                                          , className
                                                          , composeAll
                                                          , doShift
                                                          )
import           XMonad.Hooks.ManageHelpers               ( (-?>)
                                                          , composeOne
                                                          , doCenterFloat
                                                          , doFullFloat
                                                          , isDialog
                                                          , isFullscreen
                                                          , transience'
                                                          )
import           XMonad.Util.NamedScratchpad              ( namedScratchpadManageHook )

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
                       [ matchQuery -- send windows to designated ws
                           className
                           [ ((`oneOf` ["Firefox", "Chromium-browser"]), shiftIfFoundWS "www")
                           , ((`oneOf` ["Hexchat", "Gitter", "Element"]), shiftIfFoundWS "chat")
                           , ((=? "vlc")                     , shiftIfFoundWS "media")
                           , ((`oneOf` ["VirtualBox", "VirtualBox Manager"]), shiftIfFoundWS "vbox")
                           ]
                       , namedScratchpadManageHook myScratchPads
                       , isFullscreen --> doFullFloat
                       , isDialog
                       <||>    className
                       `oneOf` [ "Alacritty Float"
                               , "Nm-connection-editor"
                               , "Pavucontrol"
                               , "Xmessage"
                               , "Gxmessage"
                               , "VirtualBox"
                               , "VirtualBox Manager"
                               , "Gimp"
                               , "Gimp-2.10"
                               , "Nitrogen"
                               , "Viewnior"
                               , "Sxiv"
                               ]
                       -->     doCenterFloat
                       , transience' -- send transient window to its parent
                       ]
    }
  where
    shiftIfFoundWS :: WorkspaceId -> ManageHook
    shiftIfFoundWS ws = maybe mempty doShift . find (== ws) $ myWorkspaces
    oneOf :: (Foldable t, Eq a) => Query a -> t a -> Query Bool
    oneOf needle hay = fmap (`elem` hay) needle
    matchQuery :: Query a -> [(Query a -> Query Bool, ManageHook)] -> ManageHook
    matchQuery q = composeOne . fmap (\(p, hook) -> p q -?> hook)
