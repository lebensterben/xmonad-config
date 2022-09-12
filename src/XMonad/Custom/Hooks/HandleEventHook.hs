----------------------------------------------------------------------------------------------------
-- |
-- Module      : XMonad.Custom.Hooks.HandleEventHook
-- Copyright   : (c) Lucius Hu, 2020-2022
-- License     : BSD3
-- Maintainer  : Lucius Hu <lebensterben@users.noreply.github.com>
--
-- Provides interfaces to interact with \"xmonadctl\" CLI utility.
----------------------------------------------------------------------------------------------------

module XMonad.Custom.Hooks.HandleEventHook (myHandleEventHook) where

import           Data.Monoid                              ( All )
import qualified Graphics.X11.Xlib.Extras                as X11
import           XMonad                                   ( X
                                                          , io
                                                          )
import           XMonad.Hooks.ScreenCorners               ( screenCornerEventHook )
import           XMonad.Hooks.ServerMode                  ( serverModeEventHook
                                                          , serverModeEventHookCmd
                                                          , serverModeEventHookF
                                                          )
import           XMonad.Layout.LayoutHints                ( hintsEventHook )

----------------------------------------------------------------------------------------------------
-- Handle Event Hook
----------------------------------------------------------------------------------------------------

-- TODO: Doc is not complete
-- | Handles an X event, s.t. unless it returns 'All True', other handlers will be skipped.
-- Consists of following parts:
--
-- * Recevies commands from an external client
myHandleEventHook :: X11.Event -> X All
myHandleEventHook =
    serverModeEventHookCmd
        >> serverModeEventHook
        >> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn)
        >> hintsEventHook
        >> screenCornerEventHook
