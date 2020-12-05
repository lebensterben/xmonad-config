----------------------------------------------------------------------------------------------------
-- |
-- Module      : Custom.Util.Font
-- Copyright   : (c) Lucius Hu, 2020
-- License     : BSD3
-- Maintainer  : Lucius Hu <lebensterben@users.noreply.github.com>
--
-- Utilities to deal with XLFD fonts.
----------------------------------------------------------------------------------------------------

module Custom.Util.Font
    (
    -- * Utilities for dealing with fonts
      Font(..)
    , FontSet(..)
    , FontFace(..)
    , FontWeight(..)
    , FontSize(..)
    )
where

import           Data.Function                            ( (&) )

----------------------------------------------------------------------------------------------------
-- Font
----------------------------------------------------------------------------------------------------

-- | Represents a particular font. For example
--
-- >>> myFS = FontSet { faceUI = "Sans", faceTerm = "Mono", faceIcon = "Icons", sizeDef = 12 }
-- >>> myUISimpleFont = Font myFontSet UI Regular DefaultSize
-- xft:Sans-12::antialias=true:hinting=true:hintstyle=hintfull
data Font = Font
    FontSet    -- ^ Contains all information needed.
    FontFace
    FontWeight
    FontSize

instance Show Font where
    show (Font fontset face weight size) = concat
        ["xft:", f, "-", s, ":", w, ":antialias=true:hinting=true:hintstyle=hintfull"]
      where
        f = fontset & case face of
            UI   -> faceUI
            Term -> faceTerm
            Icon -> faceIcon
        s = case size of
            DefaultSize -> show $ sizeDef fontset
            FontSize i  -> show i
        w = case weight of
            Regular  -> ""
            DemiBold -> ":demibold"
            Bold     -> ":bold"

-- | Font set contains information for generating strings reprensenting XLFD Font Naming.
-- This is where users can customize their selection of fonts used across their XMonad configs.
--
-- >>> FontSet {faceUI = "Sans", faceTerm = "Mono", faceIcon = "All the icons", sizeDef = 12}
-- FontSet {faceUI = "Sans", faceTerm = "Mono", faceIcon = "All the icons", sizeDef = 12}
data FontSet = FontSet {
    faceUI :: String,   -- ^ Font face for 'UI'
    faceTerm :: String, -- ^ Font face for 'Term'
    faceIcon :: String, -- ^ Font face for 'Icon'
    sizeDef :: Int      -- ^ Font size for 'DefaultSize'
                       }
   deriving ( Show )

-- | Represents a few styles of fonts.
data FontFace = UI | Term | Icon
   deriving ( Show )

-- | Font weights.
data FontWeight = Regular | DemiBold | Bold
   deriving ( Show )

-- | Font size.
data FontSize = DefaultSize | FontSize Int
   deriving ( Show )
