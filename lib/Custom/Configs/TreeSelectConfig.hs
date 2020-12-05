----------------------------------------------------------------------------------------------------
-- |
-- Module      : Custom.Configs.TreeSelectConfig
-- Copyright   : (c) Lucius Hu, 2020
-- License     : BSD3
-- Maintainer  : Lucius Hu <lebensterben@users.noreply.github.com>
--
-- Config for tree select menu.
----------------------------------------------------------------------------------------------------

module Custom.Configs.TreeSelectConfig
    (
    -- * Tree Select Config
      treeSelectConfig
    )
where

import           Custom.Keymap                            ( additionalTSKeysP )
import           Custom.Keymap.TreeSelectKeymap           ( treeSelectKeymap )
import           Custom.Variables                         ( myColor
                                                          , myFontSet
                                                          )
import           Custom.Util.Color                        ( toARGBPix
                                                          , ColorScheme(..)
                                                          )
import           Custom.Util.Font                         ( Font(..)
                                                          , FontFace(..)
                                                          , FontWeight(..)
                                                          , FontSize(..)
                                                          )
import           XMonad                                   ( def
                                                          , XConfig
                                                          )
import qualified XMonad.Actions.TreeSelect               as TS

----------------------------------------------------------------------------------------------------
-- Tree Select Config
----------------------------------------------------------------------------------------------------

-- | Config for tree select menu.
treeSelectConfig :: XConfig l -> TS.TSConfig a
treeSelectConfig xconf = additionalTSKeysP
    xconf
    def { TS.ts_background  = bg myColor `toARGBPix` 85.0
                              -- #282c34
        , TS.ts_font        = show $ Font myFontSet UI Bold DefaultSize
        , TS.ts_node        = (fg myColor `toARGBPix` 100.0, base1 myColor `toARGBPix` 100)
                              -- @bbc2cf_1c1f24@
        , TS.ts_nodealt     = (fg myColor `toARGBPix` 100.0, bg myColor `toARGBPix` 100)
                              -- @bbc2cf_282c34@
        , TS.ts_highlight   = (base8 myColor `toARGBPix` 100.0, dimBlue myColor `toARGBPix` 100)
                              -- @dfdfdf_2c6083@
        , TS.ts_extra       = base7 myColor `toARGBPix` 100.0
                              -- #6b6d70
        , TS.ts_node_height = 25
        , TS.ts_node_width  = 230
        , TS.ts_originX     = 100
        , TS.ts_originY     = 100
        }
    treeSelectKeymap

-- Local Variables:
-- eval: (when (fboundp 'xah-syntax-color-hex) (eval '(xah-syntax-color-hex)))
-- end:
