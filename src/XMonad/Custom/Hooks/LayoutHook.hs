{-# LANGUAGE FlexibleContexts #-}
----------------------------------------------------------------------------------------------------
-- |
-- Module      : XMonad.Custom.Layouts
-- Copyright   : (c) Lucius Hu, 2020-2022
-- License     : BSD3
-- Maintainer  : Lucius Hu <lebensterben@users.noreply.github.com>
--
-- My selection of layouts.
----------------------------------------------------------------------------------------------------

module XMonad.Custom.Hooks.LayoutHook (myLayoutHook) where

import           XMonad                                   ( LayoutClass
                                                          , (|||)
                                                          )
import           XMonad.Custom.Variables                  ( mySpacingWidth
                                                          , showWNameConfig
                                                          , tabbedConfig
                                                          )
import           XMonad.Hooks.ManageDocks                 ( avoidStruts )
import           XMonad.Hooks.ScreenCorners               ( screenCornerLayoutHook )
import           XMonad.Layout.BinarySpacePartition       ( emptyBSP )
import           XMonad.Layout.BorderResize               ( borderResize )
import           XMonad.Layout.Decoration                 ( Decoration
                                                          , DefaultShrinker
                                                          , LayoutModifier
                                                          , ModifiedLayout
                                                          , Theme
                                                          , shrinkText
                                                          )
import           XMonad.Layout.GridVariants               ( Grid(Grid) )
import           XMonad.Layout.Hidden                     ( hiddenWindows )
import           XMonad.Layout.LayoutHints                ( LayoutHints
                                                          , layoutHints
                                                          )
import           XMonad.Layout.LimitWindows               ( LimitWindows
                                                          , limitWindows
                                                          )
import           XMonad.Layout.MultiToggle                ( mkToggle
                                                          , single
                                                          )
import           XMonad.Layout.MultiToggle.Instances      ( StdTransformers(NBFULL) )
import           XMonad.Layout.Renamed                    ( Rename(CutWordsLeft, Replace)
                                                          , renamed
                                                          )
import           XMonad.Layout.ResizableTile              ( ResizableTall(ResizableTall) )
import           XMonad.Layout.ShowWName                  ( showWName' )
import           XMonad.Layout.Simplest                   ( Simplest(..) )
import           XMonad.Layout.SimplestFloat              ( simplestFloat )
import           XMonad.Layout.Spacing                    ( Border(Border)
                                                          , Spacing
                                                          , spacingRaw
                                                          )
import           XMonad.Layout.SubLayouts                 ( Sublayout
                                                          , subLayout
                                                          )
import           XMonad.Layout.Tabbed                     ( TabbedDecoration
                                                          , addTabs
                                                          )
import           XMonad.Layout.ToggleLayouts              ( toggleLayouts )
import           XMonad.Layout.WindowArranger             ( windowArrange )

----------------------------------------------------------------------------------------------------
-- Utility Functions
----------------------------------------------------------------------------------------------------

-- | Same as 'XMonad.Layout.SubLayouts.subTabbed', but takes a custom theme.
subTabbed' :: (Eq a, LayoutModifier (Sublayout Simplest) a, LayoutClass l a)
           => Theme -- ^ A config for the tabbed layout.
           -> l a   -- ^ A layout on which the tabbed layout modifier is applied.
           -> ModifiedLayout
                  Rename
                  ( ModifiedLayout
                        (Decoration TabbedDecoration DefaultShrinker)
                        (ModifiedLayout (Sublayout Simplest) l)
                  )
                  a
subTabbed' c l = renamed [CutWordsLeft 1] . addTabs shrinkText c $ subLayout [] Simplest l

-- | Apply multiple common layout modifiers:
--
-- * Rename the resulting layout.
-- * Accept layout hints.
-- * Limit visible windows number to 12.
-- * Add border spacing of 4.
tilingLayout :: (Eq a, LayoutClass l a)
             => String -- ^ The name for the tiling layout.
             -> l a    -- ^ A layout on which the tiling layout modifier is applied.
             -> ModifiedLayout
                    Rename
                    ( ModifiedLayout
                          LayoutHints
                          (ModifiedLayout LimitWindows (ModifiedLayout Spacing l))
                    )
                    a
tilingLayout n l = renamed [Replace n] . layoutHints . limitWindows 12 $ spacing l
  where
    i       = mySpacingWidth
    spacing = spacingRaw False (Border i i i i) True (Border i i i i) True

----------------------------------------------------------------------------------------------------
-- Float Window Layout
----------------------------------------------------------------------------------------------------

-- | A Float layout.
--
-- * All windows are floating with no decoration.
-- * Show first 12 windows.
-- * No spacing added.
floats = renamed [Replace "Float"] . borderResize . windowArrange . layoutHints $ limitWindows
    20
    simplestFloat

----------------------------------------------------------------------------------------------------
-- Tall Layout
----------------------------------------------------------------------------------------------------

-- | A resizable Tall layout.
--
-- * 1 master window that takes left half of screen.
-- * Slave windows equally share the rest of screen.
tall = tilingLayout "Tall" $ ResizableTall 1 (3 / 100) (1 / 3) []

----------------------------------------------------------------------------------------------------
-- Grid Layout
----------------------------------------------------------------------------------------------------

-- | A Grid layout.
--
-- * Grid has 16:9 widht-to-height ratio.
grid = tilingLayout "Grid" $ Grid (16 / 9)

----------------------------------------------------------------------------------------------------
-- Binary Partition Layout
----------------------------------------------------------------------------------------------------

-- | A Binary Partition layout.
--
-- * New window is spawned at location of current focus.
-- * New window takes half of the size of current location.
binary = tilingLayout "Binary" emptyBSP

----------------------------------------------------------------------------------------------------
-- My Layouts Selection
----------------------------------------------------------------------------------------------------

-- | My selection of layouts.
--
-- * Two groups of layouts, tiling layouts and float layouts.
-- * Tiling layouts compirse of: 'tall', 'binary', 'grid'.
-- * All tiling layouts have tabbed subLayout.
myLayoutHook =
    avoidStruts
        .   showWName' showWNameConfig
        .   renamed [CutWordsLeft 1]
        .   hiddenWindows
        .   toggleLayouts floats
        .   mkToggle (single NBFULL)
        .   subTabbed' tabbedConfig
        .   screenCornerLayoutHook
        $   tall
        ||| binary
        ||| grid
