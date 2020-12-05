----------------------------------------------------------------------------------------------------
-- |
-- Module      : Custom.Hooks.LogHook
-- Copyright   : (c) Lucius Hu, 2020
-- License     : BSD3
-- Maintainer  : Lucius Hu <lebensterben@users.noreply.github.com>
--
-- Defines actions to perform when the window set is changed.
----------------------------------------------------------------------------------------------------

module Custom.Hooks.LogHook
    (
    -- * Log Hook
      myLogHook
    )
where

import           Custom.Variables                         ( inactiveWindowFadeAmount )
import           XMonad                                   ( XConfig(logHook) )
import           XMonad.Actions.SwapPromote               ( masterHistoryHook )
import           XMonad.Hooks.FadeInactive                ( fadeInactiveLogHook )
import           XMonad.Hooks.WorkspaceHistory            ( workspaceHistoryHook )

----------------------------------------------------------------------------------------------------
-- Log Hook
----------------------------------------------------------------------------------------------------

-- | Actions to perform when the window set is changed.
--
-- * 'workspaceHistoryHook': Keeps track of the order in which workspaces are viewed.
-- * 'fadeInactiveLogHook': Makes inactive window a bit dimmer.
-- TODO: history hook
myLogHook :: XConfig l -> XConfig l
myLogHook conf = conf
    { logHook = workspaceHistoryHook
                >> fadeInactiveLogHook inactiveWindowFadeAmount
                >> masterHistoryHook
    }
