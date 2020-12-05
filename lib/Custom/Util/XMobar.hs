{-# LANGUAGE FlexibleContexts, LambdaCase #-}
----------------------------------------------------------------------------------------------------
-- |
-- Module      : Custom.Util.XMobar
-- Copyright   : (c) Lucius Hu, 2020
-- License     : BSD3
-- Maintainer  : Lucius Hu <lebensterben@users.noreply.github.com>
--
-- Utility functions for XMobar integration.
----------------------------------------------------------------------------------------------------

module Custom.Util.XMobar
    (
    -- * XMobar Integration
      xmobarEscape
    , xmobarMulti
    , XMobarConfig(..)
    )
where

import           Data.List                                ( partition )
import           Data.Maybe                               ( catMaybes )
import           XMonad                                   ( Window
                                                          , XConfig
                                                              ( layoutHook
                                                              , startupHook
                                                              , logHook
                                                              )
                                                          , LayoutClass
                                                          )
import           XMonad.Hooks.ManageDocks                 ( avoidStruts
                                                          , docks
                                                          , AvoidStruts
                                                          )
import           XMonad.Hooks.DynamicLog                  ( dynamicLogWithPP
                                                          , PP(..)
                                                          )
import           XMonad.Layout.LayoutModifier             ( ModifiedLayout(..) )
import           XMonad.Util.Run                          ( hPutStrLn
                                                          , spawnPipe
                                                          )
import           XMonad.Util.SpawnNamedPipe               ( getNamedPipe
                                                          , spawnNamedPipe
                                                          )

----------------------------------------------------------------------------------------------------
-- XMobar Integration
----------------------------------------------------------------------------------------------------

data XMobarConfig = PipedXMobar String String -- ^ A piped XMobar instance with a config file and
                                              --   a named handle.
                  | UnPipedXMobar String      -- ^ A simple XMobar instance with a config file
                                              --   only.


-- | Escape special characters in XMobar.
xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
  where
    doubleLts x | x == '<'  = "<<"
                | otherwise = [x]

-- | Similar to 'XMonad.Hooks.DynamicLog.xmobar' except in two aspects:
--
-- * It takes a list of XMobar config files as first arguments.
-- * It does not add the keybinding to toggle struts.
xmobarMulti :: LayoutClass l Window
            => [XMobarConfig]  -- ^ the config file(s) of xmobar
            -> PP              -- ^ the pretty printing options
            -> XConfig l       -- ^ the base config
            -> XConfig (ModifiedLayout AvoidStruts l)
xmobarMulti rc pp conf = docks $ conf
    { layoutHook  = avoidStruts (layoutHook conf) -- prevent new window covering DOCK
    , startupHook = startupHook conf >> unpipedXMobar >> pipedXMobar
    , logHook     = do
        handles' <- mapM (\ ~(PipedXMobar _ p) -> getNamedPipe p) piped
        let handles = catMaybes handles'
        logHook conf >> dynamicLogWithPP pp { ppOutput = \x -> mapM_ (`hPutStrLn` x) handles }
    }
  where
    (piped, unpiped) = partition
        (\case
            PipedXMobar _ _ -> True
            _               -> False
        )
        rc
    unpipedXMobar = mapM_ (\ ~(UnPipedXMobar c) -> spawnPipe $ "xmobar " ++ c) unpiped
    pipedXMobar   = mapM_ (\ ~(PipedXMobar c p) -> spawnNamedPipe ("xmobar " ++ c) p) piped
