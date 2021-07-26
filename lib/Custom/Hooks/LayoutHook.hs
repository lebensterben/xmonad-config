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
import           Custom.Layouts                           ( ComplexBinaryPartition
                                                          , ComplexFloat
                                                          , ComplexGrid
                                                          , ComplexTall
                                                          , LayoutSelection
                                                          , floats
                                                          , myLayouts
                                                          )
import           XMonad                                   ( XConfig(layoutHook) )
import           XMonad.Layout.LayoutModifier             ( ModifiedLayout )
import           XMonad.Layout.MultiToggle                ( (??)
                                                          , EOT(EOT)
                                                          , HCons
                                                          , MultiToggle
                                                          , mkToggle
                                                          )
import           XMonad.Layout.MultiToggle.Instances      ( StdTransformers(NBFULL, NOBORDERS) )
import           XMonad.Layout.ShowWName                  ( ShowWName
                                                          , showWName'
                                                          )
import           XMonad.Layout.ToggleLayouts              ( ToggleLayouts
                                                          , toggleLayouts
                                                          )

myLayoutHook :: XConfig l
             -> XConfig
                    ( ModifiedLayout
                          ShowWName
                          ( ToggleLayouts
                                ComplexFloat
                                ( MultiToggle
                                      (HCons StdTransformers (HCons StdTransformers EOT))
                                      ( LayoutSelection
                                            ComplexTall
                                            ComplexBinaryPartition
                                            ComplexGrid
                                      )
                                )
                          )
                    )
myLayoutHook conf = conf
    { layoutHook = showWName' showWNameConfig . toggleLayouts floats $ mkToggle
                       (NBFULL ?? NOBORDERS ?? EOT)
                       myLayouts
    }
