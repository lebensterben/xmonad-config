----------------------------------------------------------------------------------------------------
-- |
-- Module      : Custom.Configs.WindowNavigationHook
-- Copyright   : (c) Lucius Hu, 2020
-- License     : BSD3
-- Maintainer  : Lucius Hu <lebensterben@users.noreply.github.com>
--
-- Provides 'windowNavigationHook' that modifies the directional window navigation strategies.
----------------------------------------------------------------------------------------------------

module Custom.Hooks.WindowNavigationHook
    (
    -- * Window Navigation Hook
      windowNavigationHook
    )
where

import           XMonad                                   ( XConfig
                                                          , def
                                                          )
import           XMonad.Actions.Navigation2D              ( centerNavigation
                                                          , hybridOf
                                                          , lineNavigation
                                                          , sideNavigation
                                                          , withNavigation2DConfig
                                                          , Navigation2DConfig(..)
                                                          )

----------------------------------------------------------------------------------------------------
-- Window Navigation Hook
----------------------------------------------------------------------------------------------------

-- | Modifes the directional window navigation strategies. Uses hybrid strategies suggested
-- in "XMonad.Actions.Navigation2D":
--
-- * 'defaultTiledNavigation': A hybrid of 'sideNavigation' and 'centerNavigation'.
-- * 'floatNavigation': A hybrid of 'lineNavigation' and 'centerNavigation'.
windowNavigationHook :: XConfig a -> XConfig a
windowNavigationHook = withNavigation2DConfig def
    { defaultTiledNavigation = hybridOf sideNavigation centerNavigation
    , floatNavigation        = hybridOf lineNavigation centerNavigation
    }
