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
    , BorderKind(..)
    )
where

import           Custom.Util.Color                        ( ColorScheme(..) )
import           Custom.Util.Font                         ( Font(..)
                                                          , FontFace(..)
                                                          , FontWeight(..)
                                                          , FontSize(..)
                                                          )
import           Custom.Variables                         ( myBorderWidth
                                                          , myColor
                                                          , myFontSet
                                                          , mySpacingWidth
                                                          )
import           XMonad                                   ( def )
import           XMonad.Layout.Decoration                 ( Theme(..) )
import           XMonad.Layout.LayoutModifier             ( ModifiedLayout )
import           XMonad.Layout.Spacing                    ( spacingRaw
                                                          , Border(..)
                                                          , Spacing
                                                          )

----------------------------------------------------------------------------------------------------
-- Tabbed Config
----------------------------------------------------------------------------------------------------

-- | Config for Tabbed (sub-)layout.
tabbedConfig :: Theme
tabbedConfig = def { activeColor         = bg myColor     -- #282c34
                   , inactiveColor       = base6 myColor  -- #5b6268
                   , urgentColor         = orange myColor -- #da8548
                   , activeBorderWidth   = myBorderWidth
                   , inactiveBorderWidth = myBorderWidth
                   , urgentBorderWidth   = myBorderWidth
                   , activeTextColor     = base8 myColor  -- #dfdfdf
                   , inactiveTextColor   = fg myColor     -- #bbc2cf
                   , urgentTextColor     = base8 myColor  -- #dfdfdf
                   , fontName            = show $ Font myFontSet UI Regular DefaultSize
                   }

----------------------------------------------------------------------------------------------------
-- Border Spacing
----------------------------------------------------------------------------------------------------

-- | Represents with a window has smart border enabled.
data BorderKind
    = Naive -- ^ No smart border.
    | Smart -- ^ With smart border, i.e. borders only appear when more than 1 window.
    deriving (Eq)

-- | Add spacing to screen border and window border. Optionally enable smart border so a single
-- window won't have gaps.
spacing :: BorderKind -- ^ Enable smart border
        -> l a
        -> ModifiedLayout Spacing l a
spacing smart = spacingRaw (smart == Smart) (Border i i i i) True (Border i i i i) True
    where i = mySpacingWidth

-- Local Variables:
-- eval: (when (fboundp 'xah-syntax-color-hex) (eval '(xah-syntax-color-hex)))
-- end:
