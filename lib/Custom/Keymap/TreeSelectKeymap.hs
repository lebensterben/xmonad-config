module Custom.Keymap.TreeSelectKeymap (treeSelectKeymap) where

import qualified XMonad.Actions.TreeSelect               as TS
                                                          ( moveChild
                                                          , moveTo
                                                          )

-- TODO add description
treeSelectKeymap =
    [ ("a"     , TS.moveTo ["+ Accessories"] >> TS.moveChild)
    , ("g"     , TS.moveTo ["+ Graphics"])
    , ("i"     , TS.moveTo ["+ Internet"])
    , ("m"     , TS.moveTo ["+ Multimedia"])
    , ("o"     , TS.moveTo ["+ Office"])
    , ("p"     , TS.moveTo ["+ Programming"])
    , ("s"     , TS.moveTo ["+ System"])
    , ("b"     , TS.moveTo ["+ Bookmarks"])
    , ("c"     , TS.moveTo ["+ Config Files"])
    , ("r"     , TS.moveTo ["+ Screenshots"])
    , ("x"     , TS.moveTo ["+ XMonad"])
    , ("M-l"   , TS.moveTo ["+ Bookmarks", "+ Linux"])
    , ("M-e"   , TS.moveTo ["+ Bookmarks", "+ Emacs"])
    , ("M-s"   , TS.moveTo ["+ Bookmarks", "+ Search and Reference"])
    , ("M-p"   , TS.moveTo ["+ Bookmarks", "+ Programming"])
    , ("M-v"   , TS.moveTo ["+ Bookmarks", "+ Vim"])
    , ("M-M1-c", TS.moveTo ["+ Bookmarks", "+ Linux", "+ Clear Linux"])
    , ("M-M1-n", TS.moveTo ["+ Bookmarks", "+ Linux", "+ Linux News"])
    , ("M-M1-x", TS.moveTo ["+ Bookmarks", "+ Linux", "+ XMonad"])
    , ("M-M1-o", TS.moveTo ["+ Bookmarks", "+ Linux", "+ Others"])
    ]
