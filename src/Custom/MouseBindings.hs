----------------------------------------------------------------------------------------------------
-- |
-- Module      : Custom.MouseBindings
-- Copyright   : (c) Lucius Hu, 2020-2022
-- License     : BSD3
-- Maintainer  : Lucius Hu <lebensterben@users.noreply.github.com>
--
-- Defines mouse bindings.
----------------------------------------------------------------------------------------------------

module Custom.MouseBindings (myMouseBindings) where

import qualified Data.Map                                as M
import           XMonad                                   ( (.|.)
                                                          , Button
                                                          , KeyMask
                                                          , Window
                                                          , X
                                                          , XConfig(..)
                                                          , button1
                                                          , button3
                                                          , focus
                                                          , mouseMoveWindow
                                                          , mouseResizeWindow
                                                          , shiftMask
                                                          )
import           XMonad.Actions.FloatSnap                 ( Direction2D(D, R)
                                                          , afterDrag
                                                          , snapMagicMove
                                                          , snapMagicResize
                                                          )
import           XMonad.Actions.TiledWindowDragging       ( dragWindow )

-- | Defines the mouse bindings.
--
-- * To make a window floating and move it, hold @Mod@ key and drag it with left mouse button.
-- * To change the size of a floating window, hold @Mod@ key drag with right mouse button.
-- * Both actions will let the window gain focus. They also make it a floating window, if it is not
-- already. Furthermore, after movement/resizing, it's snapped to the closest window border or
-- screen edge.
myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig { modMask = myModMask } = M.fromList
    [ ( (myModMask, button1)
      , \w -> focus w >> mouseMoveWindow w >> afterDrag (snapMagicMove (Just 50) (Just 50) w)
      )
    , ((myModMask .|. shiftMask, button1), dragWindow)
    , ( (myModMask, button3)
      , \w -> focus w >> mouseResizeWindow w >> afterDrag
          (snapMagicResize [R, D] (Just 50) (Just 50) w)
      )
    ]
