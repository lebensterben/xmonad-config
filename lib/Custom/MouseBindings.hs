-- TODO: doc
module Custom.MouseBindings (defineMouseBindings, myMouseBindings) where

import qualified Data.Map                                as M
import           XMonad                                   ( Button
                                                          , ButtonMask
                                                          , KeyMask
                                                          , Layout
                                                          , Window
                                                          , X
                                                          , XConfig(XConfig, modMask)
                                                          , button1
                                                          , button3
                                                          , focus
                                                          , mouseBindings
                                                          , mouseMoveWindow
                                                          , mouseResizeWindow
                                                          )
import           XMonad.Actions.FloatSnap                 ( Direction2D(D, R)
                                                          , afterDrag
                                                          , snapMagicMove
                                                          , snapMagicResize
                                                          )
defineMouseBindings :: (XConfig Layout -> M.Map (ButtonMask, Button) (Window -> X ()))
                    -> XConfig l
                    -> XConfig l
defineMouseBindings bindings conf = conf { mouseBindings = bindings }

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
