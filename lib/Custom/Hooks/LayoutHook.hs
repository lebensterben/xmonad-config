----------------------------------------------------------------------------------------------------
-- |
-- Module      : Custom.Hooks.LayoutHook
-- Copyright   : (c) Lucius Hu, 2020
-- License     : BSD3
-- Maintainer  : Lucius Hu <lebensterben@users.noreply.github.com>
--
-- Layout Hook. TODO
----------------------------------------------------------------------------------------------------

module Custom.Hooks.LayoutHook (myLayoutHook) where

import           Custom.Configs.ShowWNameConfig           ( showWNameConfig )
import           Custom.Layouts                           ( ComplexGrid
                                                          , ComplexBinaryPartition
                                                          , ComplexThreeCol
                                                          , ComplexTall
                                                          , LayoutSelection
                                                          , ComplexFloat
                                                          , floats
                                                          , myLayouts
                                                          )
import           Custom.ShowWName                         ( ShowWNameEscape
                                                          , showWNameEscape
                                                          )
import           XMonad                                   ( XConfig(layoutHook) )
import           XMonad.Layout.LayoutModifier             ( ModifiedLayout )
import           XMonad.Layout.MultiToggle                ( HCons
                                                          , MultiToggle
                                                          , mkToggle
                                                          , (??)
                                                          , EOT(EOT)
                                                          )
import           XMonad.Layout.MultiToggle.Instances      ( StdTransformers(NBFULL, NOBORDERS) )
import           XMonad.Layout.ToggleLayouts              ( ToggleLayouts
                                                          , toggleLayouts
                                                          )

myLayoutHook :: XConfig l
             -> XConfig
                    ( ModifiedLayout
                          ShowWNameEscape
                          ( ToggleLayouts
                                ComplexFloat
                                ( MultiToggle
                                      (HCons StdTransformers (HCons StdTransformers EOT))
                                      ( LayoutSelection
                                            ComplexTall
                                            ComplexThreeCol
                                            ComplexBinaryPartition
                                            ComplexGrid
                                      )
                                )
                          )
                    )
myLayoutHook conf = conf
    { layoutHook = showWNameEscape showWNameConfig . toggleLayouts floats $ mkToggle
                       (NBFULL ?? NOBORDERS ?? EOT)
                       myLayouts
    }
