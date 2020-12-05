{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
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
    , ComplexThreeCol
    , ComplexBinaryPartition
    , ComplexGrid

    -- * Collection Types
    , myLayouts
    , LayoutSelection
    )
where

import           Custom.Configs.LayoutConfig              ( spacing
                                                          , tabbedConfig
                                                          , BorderKind(Naive, Smart)
                                                          )
import           XMonad                                   ( Window
                                                          , LayoutClass
                                                          )
import           XMonad.Layout.BinarySpacePartition       ( emptyBSP
                                                          , BinarySpacePartition
                                                          )
import           XMonad.Layout.BorderResize               ( borderResize
                                                          , BorderResize
                                                          )
import           XMonad.Layout.Decoration                 ( Decoration
                                                          , DefaultShrinker
                                                          , ModifiedLayout
                                                          )
import           XMonad.Layout.GridVariants               ( Grid(Grid) )
import           XMonad.Layout.LayoutCombinators          ( (|||)
                                                          , NewSelect
                                                          )
import           XMonad.Layout.LayoutHints                ( layoutHints
                                                          , LayoutHints
                                                          )
import           XMonad.Layout.LayoutModifier             ( LayoutModifier )
import           XMonad.Layout.LimitWindows               ( limitWindows
                                                          , LimitWindows
                                                          )
import           XMonad.Layout.Renamed                    ( renamed
                                                          , Rename(Replace)
                                                          )
import           XMonad.Layout.ResizableTile              ( ResizableTall(ResizableTall) )
import           XMonad.Layout.Simplest                   ( Simplest(Simplest) )
import           XMonad.Layout.SimplestFloat              ( simplestFloat
                                                          , SimplestFloat
                                                          )
import           XMonad.Layout.Spacing                    ( Spacing )
import           XMonad.Layout.SubLayouts                 ( subLayout
                                                          , Sublayout
                                                          )
import           XMonad.Layout.Tabbed                     ( addTabs
                                                          , shrinkText
                                                          , TabbedDecoration
                                                          )
import           XMonad.Layout.ThreeColumns               ( ThreeCol(ThreeCol) )
import           XMonad.Layout.WindowArranger             ( windowArrange
                                                          , WindowArranger
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
-- * Limit visible windows number.
-- * Add border spacing.
complexModifiers :: ( Eq a
                    , LayoutModifier (Sublayout Simplest) a
                    , LayoutModifier LayoutHints a
                    , LayoutClass l a
                    )
                 => String     -- ^ Rename.
                 -> Int        -- ^ Max number of visible windows.
                 -> BorderKind -- ^ Whether to add smart borders.
                 -> l a
                 -> ComplexLayout l a
complexModifiers n wn b l =
    renamed [Replace n]
        . addTabs shrinkText tabbedConfig
        . subLayout [] Simplest
        . layoutHints
        . limitWindows wn
        $ spacing b l

-- | Combination of layouts I use.
--
-- __NOTE__: This is not generic at all.
type LayoutSelection l1 l2 l3 l4 = NewSelect l1 (NewSelect l2 (NewSelect l3 l4))

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
tall = complexModifiers "tall" 12 Smart $ ResizableTall 1 (3 / 100) (1 / 2) []

----------------------------------------------------------------------------------------------------
-- Grid Layout
----------------------------------------------------------------------------------------------------

type ComplexGrid = ComplexLayout Grid

-- | A Grid layout.
--
-- * Grid has 16:9 widht-to-height ratio.
-- * Has taabbed sublayout.
-- * Show first 12 windows.
-- * Simple spacing with width 4.
grid :: ComplexGrid Window
grid = complexModifiers "grid" 12 Smart $ Grid (16 / 9)

----------------------------------------------------------------------------------------------------
-- Three Columns Layout
----------------------------------------------------------------------------------------------------

type ComplexThreeCol = ComplexLayout ThreeCol

-- | A ThreeColumns layout.
--
-- * Similar to Tall but has one 3 column.
-- * Has taabbed sublayout.
-- * Show first 12 windows.
-- * Simple spacing with width 4.
threeCol :: ComplexThreeCol Window
threeCol = complexModifiers "threeCol" 7 Smart $ ThreeCol 1 (3 / 100) (1 / 2)

----------------------------------------------------------------------------------------------------
-- Binary Partition Layout
----------------------------------------------------------------------------------------------------

type ComplexBinaryPartition = ComplexLayout BinarySpacePartition

-- | A Binary Partition layout.
--
-- * New window is spawned at location of current focus.
-- * New window takes half of the size of current location.
-- * Has taabbed sublayout.
-- * Show first 12 windows.
-- * Smart spacing with width 4.
binary :: ComplexBinaryPartition Window
binary = complexModifiers "binary" 12 Naive emptyBSP

----------------------------------------------------------------------------------------------------
-- My Layouts Selection
----------------------------------------------------------------------------------------------------

-- | My selection of layouts.
--
-- * 'tall'
-- * 'threeCol'
-- * 'binary'
-- * 'grid'
myLayouts :: LayoutSelection ComplexTall ComplexThreeCol ComplexBinaryPartition ComplexGrid Window
myLayouts = tall ||| threeCol ||| binary ||| grid
