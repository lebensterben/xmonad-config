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

import           Control.Monad                            ( (>=>) )
import           Custom.Variables                         ( myScratchPads )
import           Data.Function                            ( (&) )
import           Data.Functor                             ( (<&>) )
import qualified Data.HashMap.Strict                     as M
import qualified Data.HashSet                            as S
import           Data.List                                ( find )
import           Data.Maybe                               ( fromJust )
import           XMonad                                   ( (-->)
                                                          , (<||>)
                                                          , ManageHook
                                                          , Query
                                                          , WorkspaceId
                                                          , className
                                                          , composeAll
                                                          , doShift
                                                          )
import           XMonad.Hooks.ManageHelpers               ( (-->>)
                                                          , (</=?)
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
shiftWSClassses :: M.HashMap String WorkspaceId
shiftWSClassses = M.fromList
    [ ("Chromium-browser"  , "www")
    , ("Firefox"           , "www")
    , ("firefox"           , "www")
    , ("Element"           , "chat")
    , ("Gitter"            , "chat")
    , ("Hexchat"           , "chat")
    , ("vlc"               , "media")
    , ("VirtualBox"        , "vbox")
    , ("VirtualBox Manager", "vbox")
    ]

centerFloatClasses :: S.HashSet String
centerFloatClasses = S.fromList
    [ "1Password"
    , "Alacritty Float"
    , "fcitx5-config-qt"
    , "Font Manager"
    , "Gxmessage"
    , "Kvantum Manager"
    , "Nitrogen"
    , "Nm-connection-editor"
    , "Pavucontrol"
    , "qt5ct"
    , "VirtualBox"
    , "VirtualBox Manager"
    , "Gimp"
    , "Gimp-2.10"
    , "Gwe"
    , "Sxiv"
    , "Viewnior"
    , "Xmessage"
    , "Zeal"
    ]

-- | Shift a window to a workspace when both of the followings are true:
--
-- * It's class name matches a record in 'shiftWSClassses'.
-- * The specified workspace is present in current @XConfig@.
shiftToMatchedWS :: Query String -> [WorkspaceId] -> ManageHook
shiftToMatchedWS cname wss = cname <&> matchClassName & filterAndShift
  where
    matchClassName :: String -> Maybe WorkspaceId
    matchClassName = (`M.lookup` shiftWSClassses) >=> (`find` wss) . (==)

    filterAndShift :: Query (Maybe WorkspaceId) -> ManageHook
    filterAndShift = (-->> doShift . fromJust) . (</=? Nothing)

-- | Actions to run when a new window is opened.
--
-- * If the new window wants to fill the fullscreen, makes it a float window
-- * If the new window is a dialog, make it a centered float window
-- * If the new window is transient, sends it to its parent window
-- * If certain 'XMonad.Query' returns true, sent the window to a given workspace.
myManageHook :: [WorkspaceId] -> ManageHook
myManageHook wss = composeAll
    [ shiftToMatchedWS className wss
    , namedScratchpadManageHook myScratchPads
    , isFullscreen --> doFullFloat
    , isDialog <||> fmap (`S.member` centerFloatClasses) className --> doCenterFloat
    , transience' -- send transient window to its parent
    ]
