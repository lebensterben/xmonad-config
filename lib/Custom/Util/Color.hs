----------------------------------------------------------------------------------------------------
-- |
-- Module      : Custom.Util.Color
-- Copyright   : (c) Lucius Hu, 2020
-- License     : BSD3
-- Maintainer  : Lucius Hu <lebensterben@users.noreply.github.com>
--
-- Utilities to deal with colors.
----------------------------------------------------------------------------------------------------

module Custom.Util.Color where

import           Control.Arrow                            ( Arrow((&&&), arr) )
import           Data.Bits                                ( Bits(rotateR, shift, shiftR) )
import           Data.Word                                ( Word32
                                                          , Word8
                                                          )
import           XMonad                                   ( Pixel )
import           XMonad.Hooks.DynamicLog                  ( xmobarColor )

----------------------------------------------------------------------------------------------------
-- Color Scheme
----------------------------------------------------------------------------------------------------

type HexColor = String

-- | The color scheme, based on
--
-- https://raw.githubusercontent.com/hlissner/emacs-doom-themes/master/themes/doom-one-theme.el
data ColorScheme = ColorScheme {
    bg            :: HexColor, -- ^ Background color
    bgAlt         :: HexColor, -- ^ Alternative bacground color
    fg            :: HexColor, -- ^ Foreground color
    fgAlt         :: HexColor, -- ^ Alternative foreground color
    base0         :: HexColor, -- ^ Dim black
    base1         :: HexColor, -- ^ Black
    base2         :: HexColor, -- ^ Bright black
    base3         :: HexColor, -- ^ Dim grey
    base4         :: HexColor, -- ^ Grey
    base5         :: HexColor, -- ^ Bright grey
    base6         :: HexColor, -- ^ Dim white
    base7         :: HexColor, -- ^ White
    base8         :: HexColor, -- ^ Bright white
    red           :: HexColor,
    orange        :: HexColor,
    green         :: HexColor,
    teal          :: HexColor,
    yellow        :: HexColor,
    blue          :: HexColor,
    darkBlue      :: HexColor,
    dimBlue       :: HexColor,
    magenta       :: HexColor,
    brightMagenta :: HexColor,
    violet        :: HexColor,
    cyan          :: HexColor,
    darkCyan      :: HexColor,
    dimCyan       :: HexColor
    }

-- | Extracts a pair of specified colors from a color scheme.
infixl 4 <#?#>
(<#?#>) :: ColorScheme          -- ^ A color scheme.
        -> (Maybe (ColorScheme -> HexColor), Maybe (ColorScheme -> HexColor))
                                -- ^ Name of fg & bg colors.
        -> (HexColor, HexColor) -- ^ The pair of colors
(<#?#>) c (fc, bc) = arr t fc &&& arr t bc $ ($ c) where t = flip (maybe "")

-- | Adds XMobar color tags to string with given foreground and background colors.
infixr 3 <##+>
(<##+>) :: (HexColor, HexColor) -- ^ A pair of fg & bg colors.
        -> String               -- ^ A string to be colorized.
        -> String               -- ^ A XMobar colorized string.
(<##+>) (fc, bc) = xmobarColor fc bc

-- | Given a color scheme and specified foreground and background colors, add XMobar color tags
-- to a string, after applying other string formatters.
xmobarColorizer :: ColorScheme         -- ^ A color scheme.
                -> (Maybe (ColorScheme -> HexColor), Maybe (ColorScheme -> HexColor))
                                       -- ^ Name of fg & bg colors.
                -> (String -> String)  -- ^ A string formatter.
                -> String              -- ^ Input string.
                -> String
xmobarColorizer c (fc, bc) fmt x = (f, b) <##+> fmt x where (f, b) = c <#?#> (fc, bc)

-- | Convert a 'HexColor' to ARGB color in pixel representation, with supplied alpha value.
infixl 4 `toARGBPix`
toARGBPix :: HexColor -> Float -> Pixel
toARGBPix c a = color + shift alpha 24
  where
    alpha = fromInteger . floor $ a / 100.0 * 255.0
    color = read . (++) "0x" $ tail c

-- | Convert a 'HexColor' to a triple of 'Word8' values.
infixl 4 `toRGBTriple`
toRGBTriple :: HexColor -> (Word8, Word8, Word8)
toRGBTriple c = (r, g, b)
  where
    color :: Word32
    color = read . (++) "0x" $ tail c
    b     = fromIntegral . (`shiftR` 24) $ rotateR color 8
    g     = fromIntegral . (`shiftR` 24) $ rotateR color 16
    r     = fromIntegral . (`shiftR` 24) $ rotateR color 24

-- Local Variables:
-- eval: (when (fboundp 'xah-syntax-color-hex) (eval '(xah-syntax-color-hex)))
-- end:
