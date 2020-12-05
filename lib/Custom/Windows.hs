----------------------------------------------------------------------------------------------------
-- |
-- Module      : Custom.Windows
-- Copyright   : (c) Lucius Hu, 2020
-- License     : BSD3
-- Maintainer  : Lucius Hu <lebensterben@users.noreply.github.com>
--
-- Provides utility functions for window management.
----------------------------------------------------------------------------------------------------

module Custom.Windows
    (
    -- * Window Navigation
      focusWindow
    , swapWindow
    ,

    -- * Window Resizign
      shrinkWindowTo
    )
where

import           XMonad.Actions.Navigation2D              ( windowGo
                                                          , windowSwap
                                                          )
import           XMonad                                   ( X
                                                          , sendMessage
                                                          , Resize(Expand, Shrink)
                                                          )
import           XMonad.Layout.ResizableTile              ( MirrorResize(..) )
import           XMonad.Util.Types                        ( Direction2D(..) )

----------------------------------------------------------------------------------------------------
-- Window Navigation
----------------------------------------------------------------------------------------------------

-- | Move focus to the window in that direction.
focusWindow :: String                  -- ^ Prefix keys, ends with a \"-\", e.g. @"M-C-"@
            -> [(String, Direction2D)] -- ^ Lists of keys-directions pairs, e.g.
                                       --   @[("h", L), ("j", D)]@
            -> [(String, X ())]
focusWindow prefix bindings = [ (prefix ++ key, windowGo dir False) | (key, dir) <- bindings ]

-- | Swap focused window with another window in that direction.
swapWindow :: String                  -- ^ Prefix keys, ends with a \"-\", e.g. @"M-C-"@
           -> [(String, Direction2D)] -- ^ Lists of keys-directions pairs, e.g.
                                      --   @[("h", L), ("j", D)]@
           -> [(String, X ())]
swapWindow prefix bindings = [ (prefix ++ key, windowSwap dir False) | (key, dir) <- bindings ]

----------------------------------------------------------------------------------------------------
-- Window Resizing
----------------------------------------------------------------------------------------------------

-- | Resizing windows by shrinking the windows on the specified direction to the current focused
--   window. For example, shrinking to left meaning that windows which are left to the current
--   focus would be shrinked horizontally to left, and other would be expanded accordingly.
shrinkWindowTo :: String                  -- ^ Prefix keys, ends with a \"-\", e.g. @"M-C-"@
               -> [(String, Direction2D)] -- ^ Lists of keys-directions pairs, e.g.
               -> [(String, X ())]        --   @[("h", L), ("j", D)]@
shrinkWindowTo prefix bindings =
    [ ( prefix ++ key
      , case dir of
          L -> sendMessage Shrink
          R -> sendMessage Expand
          D -> sendMessage MirrorShrink
          U -> sendMessage MirrorExpand
      )
    | (key, dir) <- bindings
    ]
