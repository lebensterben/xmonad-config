----------------------------------------------------------------------------------------------------
-- |
-- Module      : Custom.Configs.LayoutConfig
-- Copyright   : (c) Lucius Hu, 2020
-- License     : BSD3
-- Maintainer  : Lucius Hu <lebensterben@users.noreply.github.com>
--
-- Config and utility functions for layouts.
----------------------------------------------------------------------------------------------------

module Custom.Configs.LayoutConfig
    (
    -- * Tabbed Config
      tabbedConfig

    -- * Border Spacing
    , spacing
    ) where

import           Custom.Variables                         ( base6
                                                          , base8
                                                          , colorBg
                                                          , colorFg
                                                          , myBorderWidth
                                                          , myDefaultFontSize
                                                          , mySpacingWidth
                                                          , myUIFont
                                                          , orange
                                                          )
import           XMonad                                   ( def )
import           XMonad.Layout.Decoration                 ( Theme(..) )
import           XMonad.Layout.LayoutModifier             ( ModifiedLayout )
import           XMonad.Layout.Spacing                    ( Border(..)
                                                          , Spacing
                                                          , spacingRaw
                                                          )

----------------------------------------------------------------------------------------------------
-- Tabbed Config
----------------------------------------------------------------------------------------------------

-- | Config for Tabbed (sub-)layout.
tabbedConfig :: Theme
tabbedConfig = def { activeColor         = colorBg     -- #282c34
                   , inactiveColor       = base6       -- #5b6268
                   , urgentColor         = orange      -- #da8548
                   , activeBorderWidth   = myBorderWidth
                   , inactiveBorderWidth = myBorderWidth
                   , urgentBorderWidth   = myBorderWidth
                   , activeTextColor     = base8       -- #dfdfdf
                   , inactiveTextColor   = colorFg     -- #bbc2cf
                   , urgentTextColor     = base8       -- #dfdfdf
                   , fontName            = myUIFont myDefaultFontSize "normal"
                   }

----------------------------------------------------------------------------------------------------
-- Border Spacing
----------------------------------------------------------------------------------------------------

-- | Add spacing to screen border and window border.
spacing :: l a
        -> ModifiedLayout Spacing l a
spacing = spacingRaw False (Border i i i i) True (Border i i i i) True
    where i = mySpacingWidth

-- Local Variables:
-- eval: (when (fboundp 'xah-syntax-color-hex) (eval '(xah-syntax-color-hex)))
-- end:
