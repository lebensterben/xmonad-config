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

import           Data.Bits                                ( Bits(rotateR, shiftR) )
import           Data.Word                                ( Word32
                                                          , Word8
                                                          )

----------------------------------------------------------------------------------------------------
-- Color Scheme
----------------------------------------------------------------------------------------------------
type HexColor = String

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
