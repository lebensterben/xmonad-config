{-# LANGUAGE FlexibleContexts #-}
----------------------------------------------------------------------------------------------------
-- |
-- Module      : Custom.Layouts
-- Copyright   : (c) Lucius Hu, 2020
-- License     : BSD3
-- Maintainer  : Lucius Hu <lebensterben@users.noreply.github.com>
--
-- My selection of layouts.
----------------------------------------------------------------------------------------------------

module Custom.Layouts
    (
    -- * Float Layout
      floats
    , ComplexFloat

    -- * Tiling Layout
    , ComplexTall
    , ComplexBinaryPartition
    , ComplexGrid

    -- * Collection Types
    , myLayouts
    , LayoutSelection
    ) where

import           Custom.Configs.LayoutConfig              ( spacing
                                                          , tabbedConfig
                                                          )
import           XMonad                                   ( LayoutClass
                                                          , Window
                                                          )
import           XMonad.Layout.BinarySpacePartition       ( BinarySpacePartition
                                                          , emptyBSP
                                                          )
import           XMonad.Layout.BorderResize               ( BorderResize
                                                          , borderResize
                                                          )
import           XMonad.Layout.Decoration                 ( Decoration
                                                          , DefaultShrinker
                                                          , ModifiedLayout
                                                          )
import           XMonad.Layout.GridVariants               ( Grid(Grid) )
import           XMonad.Layout.LayoutCombinators          ( NewSelect
                                                          , (|||)
                                                          )
import           XMonad.Layout.LayoutHints                ( LayoutHints
                                                          , layoutHints
                                                          )
import           XMonad.Layout.LayoutModifier             ( LayoutModifier )
import           XMonad.Layout.LimitWindows               ( LimitWindows
                                                          , limitWindows
                                                          )
import           XMonad.Layout.Renamed                    ( Rename(Replace)
                                                          , renamed
                                                          )
import           XMonad.Layout.ResizableTile              ( ResizableTall(ResizableTall) )
import           XMonad.Layout.Simplest                   ( Simplest(Simplest) )
import           XMonad.Layout.SimplestFloat              ( SimplestFloat
                                                          , simplestFloat
                                                          )
import           XMonad.Layout.Spacing                    ( Spacing )
import           XMonad.Layout.SubLayouts                 ( Sublayout
                                                          , subLayout
                                                          )
import           XMonad.Layout.Tabbed                     ( TabbedDecoration
                                                          , addTabs
                                                          , shrinkText
                                                          )
import           XMonad.Layout.WindowArranger             ( WindowArranger
                                                          , windowArrange
                                                          )

----------------------------------------------------------------------------------------------------
-- Utility Functions and Types
----------------------------------------------------------------------------------------------------

-- TODO
-- type ModifiedLayouts

-- | Aliase to a series of layout modifiers, used for shortening function signature.
type ComplexLayout l
    = ModifiedLayout
          Rename
          ( ModifiedLayout
                (Decoration TabbedDecoration DefaultShrinker)
                ( ModifiedLayout
                      (Sublayout Simplest)
                      ( ModifiedLayout
                            LayoutHints
                            (ModifiedLayout LimitWindows (ModifiedLayout Spacing l))
                      )
                )
          )

-- | Apply multiple common layout modifiers:
--
-- * Rename the resulting layout.
-- * Add tabbed subLayout.
-- * Accept layout hints.
-- * Limit visible windows number to 12.
-- * Add border spacing.
complexModifiers :: ( Eq a
                    , LayoutModifier (Sublayout Simplest) a
                    , LayoutModifier LayoutHints a
                    , LayoutClass l a
                    )
                 => String     -- ^ Rename.
                 -> l a
                 -> ComplexLayout l a
complexModifiers n l =
    renamed [Replace n]
        . addTabs shrinkText tabbedConfig
        . subLayout [] Simplest
        . layoutHints
        . limitWindows 12
        $ spacing l

-- | Combination of layouts I use.
--
-- __NOTE__: This is not generic at all.
type LayoutSelection l1 l2 l3 = NewSelect l1 (NewSelect l2 l3)

----------------------------------------------------------------------------------------------------
-- Float Window Layout
----------------------------------------------------------------------------------------------------

type ComplexFloat
    = ModifiedLayout
          Rename
          ( ModifiedLayout
                BorderResize
                ( ModifiedLayout
                      WindowArranger
                      ( ModifiedLayout
                            (Decoration TabbedDecoration DefaultShrinker)
                            ( ModifiedLayout
                                  (Sublayout Simplest)
                                  ( ModifiedLayout
                                        LayoutHints
                                        ( ModifiedLayout
                                              LimitWindows
                                              (ModifiedLayout WindowArranger SimplestFloat)
                                        )
                                  )
                            )
                      )
                )
          )

-- | A Float layout.
--
-- * All windows are floating with no decoration.
-- * Has taabbed sublayout.
-- * Show first 12 windows.
-- * No spacing added.
floats :: ComplexFloat Window
floats =
    renamed [Replace "floats"]
        . borderResize
        . windowArrange
        . addTabs shrinkText tabbedConfig
        . subLayout [] Simplest
        . layoutHints
        $ limitWindows 20 simplestFloat

----------------------------------------------------------------------------------------------------
-- Tall Layout
----------------------------------------------------------------------------------------------------

type ComplexTall = ComplexLayout ResizableTall

-- | A resizable Tall layout.
--
-- * 1 master window that takes left half of screen.
-- * Slave windows equally share the rest of screen.
-- * Has tabbed sublayout.
-- * Show first 12 windows.
-- * Simple spacing with width 4.

tall :: ComplexTall Window
tall = complexModifiers "tall" $ ResizableTall 1 (3 / 100) (1 / 3) []

----------------------------------------------------------------------------------------------------
-- Grid Layout
----------------------------------------------------------------------------------------------------

type ComplexGrid = ComplexLayout Grid

-- | A Grid layout.
--
-- * Grid has 16:9 widht-to-height ratio.
-- * Has tabbed sublayout.
-- * Show first 12 windows.
-- * Simple spacing with width 4.
grid :: ComplexGrid Window
grid = complexModifiers "grid" $ Grid (16 / 9)

----------------------------------------------------------------------------------------------------
-- Binary Partition Layout
----------------------------------------------------------------------------------------------------

type ComplexBinaryPartition = ComplexLayout BinarySpacePartition

-- | A Binary Partition layout.
--
-- * New window is spawned at location of current focus.
-- * New window takes half of the size of current location.
-- * Has tabbed sublayout.
-- * Show first 12 windows.
-- * Simple spacing with width 4.
binary :: ComplexBinaryPartition Window
binary = complexModifiers "binary" emptyBSP

----------------------------------------------------------------------------------------------------
-- My Layouts Selection
----------------------------------------------------------------------------------------------------

-- | My selection of layouts.
--
-- * 'tall'
-- * 'binary'
-- * 'grid'
myLayouts :: LayoutSelection ComplexTall ComplexBinaryPartition ComplexGrid Window
myLayouts = tall ||| binary ||| grid
