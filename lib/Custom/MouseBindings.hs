module Custom.MouseBindings (installMouseBindings, myMouseBindings) where

import qualified Data.Map                                as M
import           XMonad                                   ( ButtonMask
                                                          , mouseBindings
                                                          , Layout
                                                          , X
                                                          , Window
                                                          , Button
                                                          , KeyMask
                                                          , button1
                                                          , button2
                                                          , button3
                                                          , XConfig(XConfig, modMask)
                                                          , focus
                                                          , mouseMoveWindow
                                                          , mouseResizeWindow
                                                          )
import           XMonad.Actions.FloatSnap                 ( Direction2D(D, L, U, R)
                                                          , afterDrag
                                                          , snapMagicMove
                                                          , snapMagicResize
                                                          )
installMouseBindings :: XConfig l
                     -> (XConfig Layout -> [((ButtonMask, Button), Window -> X ())])
                     -> XConfig l
installMouseBindings conf mouseBindingsList =
    conf { mouseBindings = M.fromList . mouseBindingsList }

myMouseBindings :: XConfig l -> [((KeyMask, Button), Window -> X ())]
myMouseBindings XConfig { modMask = myModMask } =
    [ ( (myModMask, button1)
      , \w -> focus w >> mouseMoveWindow w >> afterDrag (snapMagicMove (Just 50) (Just 50) w)
      )
    , ( (myModMask, button2)
      , \w -> focus w >> mouseMoveWindow w >> afterDrag
          (snapMagicResize [L, R, U, D] (Just 50) (Just 50) w)
      )
    , ( (myModMask, button3)
      , \w ->
          focus w >> mouseResizeWindow w >> afterDrag (snapMagicResize [R, D] (Just 50) (Just 50) w)
      )
    ]
