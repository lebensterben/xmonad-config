----------------------------------------------------------------------------------------------------
-- |
-- Module      : Custom.Hooks.HandleEventHook
-- Copyright   : (c) Lucius Hu, 2020
-- License     : BSD3
-- Maintainer  : Lucius Hu <lebensterben@users.noreply.github.com>
--
-- Provides interfaces to interact with \"xmonadctl\" CLI utility.
----------------------------------------------------------------------------------------------------

module Custom.Hooks.HandleEventHook
    (
    -- * Handle Event Hook
      myHandleEventHook
    )
where

import           XMonad                                   ( (<+>)
                                                          , io
                                                          , XConfig(handleEventHook)
                                                          )
import           XMonad.Hooks.EwmhDesktops                ( fullscreenEventHook )
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
myHandleEventHook :: XConfig l -> XConfig l
myHandleEventHook conf = conf
    { handleEventHook = serverModeEventHookCmd
                        <+> serverModeEventHook
                        <+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn)
                        <+> hintsEventHook
                        <+> fullscreenEventHook
    }
