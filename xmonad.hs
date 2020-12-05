----------------------------------------------------------------------------------------------------
-- |
-- Module      : Main
-- Copyright   : (c) Lucius Hu, 2020
-- License     : BSD3
-- Maintainer  : Lucius Hu <lebensterben@users.noreply.github.com>
--
-- Contains the entry point for XMonad process.
-- Based on the xmonad configuration of Derek Taylor (DistroTube)
-- http://www.gitlab.com/dwt1/
----------------------------------------------------------------------------------------------------

module Main where

import           Custom.XConfig                           ( myXConfig )
import           XMonad.Main                              ( xmonad )

----------------------------------------------------------------------------------------------------
-- Entry point
----------------------------------------------------------------------------------------------------

-- 'myXmobar' launches the external myXmobar process and via
-- [@'Custom.XMobar.xmobarMulti'@]: Sets nice defaults for myXmobar status bar.
--
--     [@'XMonad.Core.startupHook'@]: 'XMonad.Hooks.ManageDocks.docksStartupHook'
--     [@'XMonad.Core.handleEventHook'@]:
--
--         [@'XMonad.Hooks.ManageDocks.docksEventHook'@]: Refresh the layout to avoid the new dock
--         when one appears.
--
--     [@'XMonad.Core.manageHook'@]:
--
--         [@'XMonad.Hooks.ManageDocks.manageDocks'@]: If a window is DOCK, reveals it but doesn't
--         manage it.
main :: IO ()
main = xmonad myXConfig

-- import           XMonad.Util.NamedScratchpad    ( customFloating
--                                                 , namedScratchpadAction
--                                                 , namedScratchpadManageHook
--                                                 , NamedScratchpad(NS)
--                                                 )
-- myScratchPads :: [NamedScratchpad]
-- myScratchPads =
--     [NS "terminal" spawnTerm findTerm manageTerm, NS "mocp" spawnMocp findMocp manageMocp]
--   where
--     spawnTerm  = myTerminal ++ " -n scratchpad"
--     findTerm   = resource =? "scratchpad"
--     manageTerm = customFloating $ W.RationalRect l t w h
--       where
--         h = 0.9
--         w = 0.9
--         t = 0.95 - h
--         l = 0.95 - w
--     spawnMocp  = myTerminal ++ " -n mocp 'mocp'"
--     findMocp   = resource =? "mocp"
--     manageMocp = customFloating $ W.RationalRect l t w h
--       where
--         h = 0.9
--         w = 0.9
--         t = 0.95 - h
--         l = 0.95 - w
