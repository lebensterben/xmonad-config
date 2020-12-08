----------------------------------------------------------------------------------------------------
-- |
-- Module      : Custom.GridSelect
-- Copyright   : (c) Lucius Hu, 2020
-- License     : BSD3
-- Maintainer  : Lucius Hu <lebensterben@users.noreply.github.com>
--
-- Provides various grid select menu.
----------------------------------------------------------------------------------------------------

module Custom.GridSelect
    (
    -- * Grid Select Menus
      gridSpawn
    , gridGoTo
    , gridBring
    , gridWS
    )
where

import           Custom.Configs.GridSelectConfig          ( gridConfigString
                                                          , gridConfigWindow
                                                          , gridContent
                                                          )
import           XMonad                                   ( whenJust
                                                          , withWindowSet
                                                          , windows
                                                          , X
                                                          )
import qualified XMonad.Actions.GridSelect               as GS
import           XMonad.Hooks.DynamicLog                  ( xmobarStrip )
import qualified XMonad.StackSet                         as W
import           XMonad.Util.Run                          ( safeSpawn )

----------------------------------------------------------------------------------------------------
-- Grid Select Menus
----------------------------------------------------------------------------------------------------

-- | A Grid Select menu for launching applications.
gridSpawn :: X ()
gridSpawn = GS.gridselect gridConfigString gridContent >>= (`whenJust` uncurry safeSpawn)

-- | A Grid Select menu for switching focus to window.
gridGoTo :: X ()
gridGoTo = GS.goToSelected gridConfigWindow

-- | A Grid Select menu for bringing selected window to workspace.
gridBring :: X ()
gridBring = GS.bringSelected gridConfigWindow

-- | A Grid Select menu for switching workspace.
gridWS :: X ()
gridWS = withWindowSet $ \ws -> do
    let wss = map W.tag $ W.hidden ws ++ map W.workspace (W.current ws : W.visible ws)
    GS.gridselect gridConfigString (zip (map xmobarStrip wss) wss)
        >>= flip whenJust (windows . W.greedyView)

-- Local Variables:
-- eval: (when (fboundp 'xah-syntax-color-hex) (eval '(xah-syntax-color-hex)))
-- end:
