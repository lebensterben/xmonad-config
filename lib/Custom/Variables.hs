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

module Custom.Variables where

import           Custom.Util.Polybar                      ( polybarFgColor )
import           XMonad                                   ( (=?)
                                                          , Default(def)
                                                          , Dimension
                                                          , WorkspaceId
                                                          , className
                                                          )
import           XMonad.Hooks.DynamicLog                  ( PP(..) )
import qualified XMonad.StackSet                         as S
import           XMonad.Util.NamedScratchpad              ( NamedScratchpad(NS)
                                                          , NamedScratchpads
                                                          , customFloating
                                                          , namedScratchpadFilterOutWorkspacePP
                                                          )

----------------------------------------------------------------------------------------------------
-- Font
----------------------------------------------------------------------------------------------------
myDefaultFontSize :: Float
myDefaultFontSize = 12.0

myUIFont :: Float -> String -> String
myUIFont size weight = concat ["xft:Clear Sans-", show size, ":", weight]

myTerminalFont :: String -> String -> String
myTerminalFont size weight = concat
    ["xft:Iosevka SS08-", size, ":", weight, ":antialias=true:hinting=true:hintstyle=hintfull"]

myIconFont :: String -> String -> String
myIconFont size weight = concat
    [ "xft:JetBrains Mono Nerd Font-"
    , size
    , ":"
    , weight
    , ":antialias=true:hinting=true:hintstyle=hintfull"
    ]

----------------------------------------------------------------------------------------------------
-- Color Scheme And Layout
----------------------------------------------------------------------------------------------------
colorBg, colorBgAlt, base6, base7, base8, colorFg, orange :: String
colorBg = "#282c34"
colorBgAlt = "#21242b"
-- base0 = "#1b2229"
-- base1 = "#1c1f24" -- black
-- base2 = "#23272e" -- base3
-- base3 = "#32363e" -- bg lighten 0.05
-- base4 = "#3f444a" -- grey
-- base5 = "#42444a" -- bgAlt lighten 0.15
base6 = "#5b6268" -- base5
base7 = "#6b6d70" -- black lighten 0.35
base8 = "#dfdfdf"
colorFg = "#bbc2cf"
-- colorFgAlt = "#5b6268" -- also base 5
-- red = "#ff6c6b"
orange = "#da8548"
-- green = "#98be65"
-- teal = "#4db5bd" -- unused
-- yellow = "#ccbe7b"
-- blue = "#51afef"
-- dimBlue = "#2c6083" -- blue darken 0.45
-- darkBlue = "#2257a0" -- bg highlight
-- magenta = "#c678dd"
-- brightMagenta = "#dcaeea" -- magenta lighten 0.4
-- violet = "#a9a1e1"
-- cyan = "#46d9ff"
-- darkCyan = "#5699af"
-- dimCyan = "#80b2c3" -- darkCyan lighten 0.25

inactiveWindowFadeAmount :: Rational
inactiveWindowFadeAmount = 0.92

myBorderWidth :: Dimension
myBorderWidth = 0

mySpacingWidth :: Integer
mySpacingWidth = 4

----------------------------------------------------------------------------------------------------
-- Workspaces
----------------------------------------------------------------------------------------------------

myWorkspaces :: [WorkspaceId]
myWorkspaces = ["dev", "www", "sys", "docs", "chat", "media", "vbox"]

----------------------------------------------------------------------------------------------------
-- Named Scracthpads
----------------------------------------------------------------------------------------------------

myScratchPads :: NamedScratchpads
myScratchPads =
    [ NS "terminal"
         "alacritty --class \"Alacritty NSP\",\"Alacritty NSP\""
         (className =? "Alacritty NSP")
         nspFloating
    ]
    where nspFloating = customFloating (S.RationalRect 0.2 0.2 0.6 0.6) -- left top width height

----------------------------------------------------------------------------------------------------
-- Polybar
----------------------------------------------------------------------------------------------------

-- | Config for Status Bar.
myStatusBarPP :: PP
myStatusBarPP = namedScratchpadFilterOutWorkspacePP $ def
    { ppOrder = \(_workspace : layout : _title : extra) -> layout : extra
    , ppSep   = polybarFgColor base7 " │ \n"
      --                       -- #dfdfdf
    }

-- Local Variables:
-- eval: (when (fboundp 'xah-syntax-color-hex) (eval '(xah-syntax-color-hex)))
-- end:
