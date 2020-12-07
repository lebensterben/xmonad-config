{-# LANGUAGE FlexibleContexts #-}
----------------------------------------------------------------------------------------------------
-- |
-- Module      : Custom.Variables
-- Copyright   : (c) Lucius Hu, 2020
-- License     : BSD3
-- Maintainer  : Lucius Hu <lebensterben@users.noreply.github.com>
--
-- Defines top level customizaton variables.
----------------------------------------------------------------------------------------------------

module Custom.Variables
    ( baseXConfig
    , myBorderWidth
    , inactiveWindowFadeAmount
    , myTerminal
    , myBrowser
    , myEditor
    , myColor
    , openWith
    , mySpacingWidth
    , xmobarrc
    , myXMobarPP
    , myFontSet
    , myWorkspaces
    , initializeStorage
    )
where

import           Custom.Util.Apps                         ( DefaultApps(..) )
import           Custom.Util.Color                        ( ColorScheme(..)
                                                          , xmobarColorizer
                                                          )
import           Custom.Util.Font                         ( FontSet(..) )
import           Custom.Util.XMobar                       ( xmobarClickableWSFormatter
                                                          , XMobarConfig(..)
                                                          )
import           Custom.Workspaces                        ( wsLabels
                                                          , Workspaces(..)
                                                          , WorkspacesStorage(..)
                                                          )
import           XMonad                                   ( gets
                                                          , windowset
                                                          , X
                                                          , mod4Mask
                                                          , KeyMask
                                                          , Dimension
                                                          , XConfig
                                                              ( modMask
                                                              , terminal
                                                              , borderWidth
                                                              , workspaces
                                                              )
                                                          , Default(def)
                                                          , spawn
                                                          , MonadIO
                                                          , Choose
                                                          , Full
                                                          , Mirror
                                                          , Tall
                                                          )
import           XMonad.Hooks.DynamicLog                  ( wrap
                                                          , shorten
                                                          , xmobarAction
                                                          , PP(..)
                                                          )
import qualified XMonad.StackSet                         as W
import qualified XMonad.Util.ExtensibleState             as XS

----------------------------------------------------------------------------------------------------
-- Modifier keys
----------------------------------------------------------------------------------------------------

-- | Set User-Defined Mod key \"M\" to \"Super_L\".
myModKey :: KeyMask
myModKey = mod4Mask

----------------------------------------------------------------------------------------------------
-- Font
----------------------------------------------------------------------------------------------------

myFontSet :: FontSet
myFontSet = FontSet { faceUI   = "Clear Sans"
                    , faceTerm = "Iosevka SS08"
                    , faceIcon = "JetBrains Mono Nerd faceIcon"
                    , sizeDef  = 12
                    }

----------------------------------------------------------------------------------------------------
-- Color Scheme
----------------------------------------------------------------------------------------------------

myColor :: ColorScheme
myColor = ColorScheme { bg            = "#282c34"
                      , bgAlt         = "#21242b"
                      , base0         = "#1b2229"
                      , base1         = "#1c1f24" -- black
                      , base2         = "#23272e" -- base3
                      , base3         = "#32363e" -- bg lighten 0.05
                      , base4         = "#3f444a" -- grey
                      , base5         = "#42444a" -- bgAlt lighten 0.15
                      , base6         = "#5b6268" -- base5
                      , base7         = "#6b6d70" -- black lighten 0.35
                      , base8         = "#dfdfdf"
                      , fg            = "#bbc2cf"
                      , fgAlt         = "#5b6268" -- also base 5
                      , red           = "#ff6c6b"
                      , orange        = "#da8548"
                      , green         = "#98be65"
                      , teal          = "#4db5bd" -- unused
                      , yellow        = "#ccbe7b"
                      , blue          = "#51afef"
                      , dimBlue       = "#2c6083" -- blue darken 0.45
                      , darkBlue      = "#2257a0" -- bg highlight
                      , magenta       = "#c678dd"
                      , brightMagenta = "#dcaeea" -- magenta lighten 0.4
                      , violet        = "#a9a1e1"
                      , cyan          = "#46d9ff"
                      , darkCyan      = "#5699af"
                      , dimCyan       = "#80b2c3" -- darkCyan lighten 0.25
--                    , base2         = "#202328" -- unused
--                    , base6         = "#73797e" -- unused
--                    , base7         = "#9ca0a4" -- white, unused
                      }

inactiveWindowFadeAmount :: Rational
inactiveWindowFadeAmount = 0.95

myBorderWidth :: Dimension
myBorderWidth = 0

mySpacingWidth :: Integer
mySpacingWidth = 4

----------------------------------------------------------------------------------------------------
-- Workspaces
----------------------------------------------------------------------------------------------------

myWorkspaces :: Workspaces
myWorkspaces = Workspaces
    { wsLbls    = ["dev", "www", "sys", "doc", "vbox", "chat", "mus", "vid", "gfx"]
    , formatter = xmobarClickableWSFormatter . spacing
    }
    where spacing = \(i, ws) -> (i, " " ++ ws ++ " ")

----------------------------------------------------------------------------------------------------
-- Default Apps
----------------------------------------------------------------------------------------------------

myDefaultApps :: DefaultApps
myDefaultApps = DefaultApps { myTerminal    = ["alacritty", "-e"]
                            , myBrowser     = ["firefox", ""]
                            , myFileManager = ["nautilus", ""]
                            , myEditor      = ["emacsclient -c -a=emacs", "--eval"]
                            }

openWith :: MonadIO m => (DefaultApps -> [String]) -> [String] -> m ()
openWith prog []   = spawn . head $ prog myDefaultApps
openWith prog args = spawn . unwords $ p : [ q ++ " " ++ r | r <- args ]
  where
    p = head $ prog myDefaultApps
    q = prog myDefaultApps !! 1

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

xmobarrc :: [XMobarConfig]
xmobarrc = [PipedXMobar "/home/lucius/.config/xmobar/xmobarrc" "primary"]

-- | Config for XMobar integration.
myXMobarPP :: PP
myXMobarPP = def
    { ppCurrent         = xmobarColorizer myColor (Just green, Nothing) $ wrap "[" "]"
                          -- #98be65
    , ppVisible         = xmobarColorizer myColor (Just green, Nothing) id
                          -- #98be65
    , ppHidden          = xmobarColorizer myColor (Just violet, Nothing) $ wrap "*" ""
                          -- #a9a1e1
    , ppHiddenNoWindows = xmobarColorizer myColor (Just darkCyan, Nothing) id
                          -- #5699af
    , ppUrgent          = xmobarColorizer myColor (Just orange, Nothing) $ wrap "!" "!"
                          -- #da8548
    , ppSep             = xmobarColorizer myColor (Just base7, Nothing) id "<fn=1>|</fn>"
                          -- #6b6d70
    , ppTitle           = xmobarColorizer myColor (Just fg, Nothing) $ shorten 80
                          -- #bbc2cf
    , ppLayout          = \l -> if l == "floats"
                              then xmobarAction "xdotool key super+shift+grave" "1" l
                              else xmobarAction "xdotool key super+grave" "1" l
                                                 -- Mouse-1 action
                                                 -- TODO make this a smart action
                                                 -- Toggle float if it's float, or toggle layout
    , ppOrder           = \(ws : l : t : ex) -> [ws, l] ++ ex ++ [t]
    , ppExtras          = windowCount -- Show # of window
    }
  where
    -- Get number of windows in current workspace to display in status bar.
    windowCount :: [X (Maybe String)]
    windowCount =
        [gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset]

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

initializeStorage :: X ()
initializeStorage = do
    XS.put (WorkspacesStorage $ wsLbls myWorkspaces)
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------


baseXConfig :: XConfig (Choose Tall (Choose (Mirror Tall) Full))
baseXConfig = def { modMask     = myModKey
                  , terminal    = head . myTerminal $ myDefaultApps
                  , borderWidth = myBorderWidth
                  , workspaces  = wsLabels myWorkspaces
                  }

-- Local Variables:
-- eval: (when (fboundp 'xah-syntax-color-hex) (eval '(xah-syntax-color-hex)))
-- end:
