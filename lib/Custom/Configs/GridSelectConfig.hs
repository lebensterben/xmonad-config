----------------------------------------------------------------------------------------------------
-- |
-- Module      : Custom.Configs.GridSelectConfig
-- Copyright   : (c) Lucius Hu, 2020
-- License     : BSD3
-- Maintainer  : Lucius Hu <lebensterben@users.noreply.github.com>
--
-- Configs for grid select menus. TODO
----------------------------------------------------------------------------------------------------

module Custom.Configs.GridSelectConfig (gridConfigWindow, gridConfigString, gridContent) where

import           Custom.Variables                         ( myColor
                                                          , myFontSet
                                                          )
import           Custom.Util.Color                        ( toRGBTriple
                                                          , ColorScheme(..)
                                                          )
import           Custom.Util.Font                         ( Font(..)
                                                          , FontFace(..)
                                                          , FontWeight(..)
                                                          , FontSize(..)
                                                          )
import           XMonad                                   ( Window
                                                          , X
                                                          )
import qualified XMonad.Actions.GridSelect               as GS

----------------------------------------------------------------------------------------------------
-- Grid Select Configs
----------------------------------------------------------------------------------------------------

-- | Custom Grid Select Window Colorizer
gridWindowColorizer :: Window -> Bool -> X (String, String)
gridWindowColorizer = GS.colorRangeFromClassName (toRGBTriple $ base3 myColor)
                      -- lower bound of ╮  color range of #23272e
                                                 (toRGBTriple $ base3 myColor)
                      -- upper bound of ╯   inactive  bg  #23272e
                                                 (toRGBTriple $ dimBlue myColor)
                      -- active bg                        #2c6083
                                                 (toRGBTriple $ fg myColor)
                      -- inactive fg                      #bbc2cf
                                                 (toRGBTriple $ base8 myColor)
                      -- active fg                        #dfdfdf

-- | Custom String Colorizer
gridStringColorizer :: String -> Bool -> X (String, String)
gridStringColorizer _ active = if active
    then return (dimBlue myColor, base8 myColor) -- @dfdfdf_2c6083@
    else return (base3 myColor, fg myColor)      -- @bbc2cf_23272e@

-- | Skeleton Grid Select Config.
gridConfig' :: GS.GSConfig a -> GS.GSConfig a
gridConfig' conf = conf { GS.gs_cellheight   = 40
                        , GS.gs_cellwidth    = 200
                        , GS.gs_cellpadding  = 6
                        , GS.gs_originFractX = 0.5
                        , GS.gs_originFractY = 0.5
                        , GS.gs_font         = show $ Font myFontSet UI Bold DefaultSize
                        }

-- | Custom Grid Select Config with custom colorizer 'gridStringColorizer'.
gridConfigString :: GS.GSConfig String
gridConfigString = gridConfig' $ GS.buildDefaultGSConfig gridStringColorizer

-- | Custom Grid Select Config with custom colorizer 'gridWindowColorizer'.
gridConfigWindow :: GS.GSConfig Window
gridConfigWindow = gridConfig' $ GS.buildDefaultGSConfig gridWindowColorizer

----------------------------------------------------------------------------------------------------
-- Grid Select Menu Contents
----------------------------------------------------------------------------------------------------

-- | Grid Select Menu Contents.
gridContent :: [(String, String)]
gridContent =
    [ ("Alacritty"      , "alacritty")
    , ("Characters"     , "gnome-characters")
    , ("Color Picker"   , "flatpak run nl.hjdskes.gcolor3")
    , ("Emacs"          , "emacsclient -c -a emacs")
    , ("Elements"       , "flatpak run im.riot.Riot")
    , ("Files"          , "nautilus")
    , ("Firefox"        , "firefox")
    , ("Fonts"          , "gnome-font-viewer")
    , ("Gimp"           , "gimp")
    , ("Gitter"         , "flatpak run im.gitter.Gitter")
    , ("HexChat"        , "hexchat")
    , ("RStudio"        , "rstudio")
    , ("Software Center", "gnome-software")
    , ("Thunderbird"    , "thunderbird")
    , ("VScode"         , "vscode")
    , ("Zeal"           , "zeal")
    ]

-- Local Variables:
-- eval: (when (fboundp 'xah-syntax-color-hex) (eval '(xah-syntax-color-hex)))
-- end:
