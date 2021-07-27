-- TODO: doc
module Custom.MouseBindings (myMouseBindings) where

import qualified Data.Map                                as M
import           XMonad                                   ( Button
                                                          , KeyMask
                                                          , Window
                                                          , X
                                                          , XConfig(..)
                                                          , button1
                                                          , button3
                                                          , focus
                                                          , mouseMoveWindow
                                                          , mouseResizeWindow
                                                          )
import           XMonad.Actions.FloatSnap                 ( Direction2D(D, R)
                                                          , afterDrag
                                                          , snapMagicMove
                                                          , snapMagicResize
                                                          )
myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig { modMask = myModMask } = M.fromList
    [ ( (myModMask, button1)
      , \w -> focus w >> mouseMoveWindow w >> afterDrag (snapMagicMove (Just 50) (Just 50) w)
      )
    , ( (myModMask, button3)
      , \w -> focus w >> mouseResizeWindow w >> afterDrag
          (snapMagicResize [R, D] (Just 50) (Just 50) w)
      )
    ]
