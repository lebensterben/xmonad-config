{-# LANGUAGE LambdaCase #-}
----------------------------------------------------------------------------------------------------
-- |
-- Module      : Custom.Workspaces
-- Copyright   : (c) Lucius Hu, 2020
-- License     : BSD3
-- Maintainer  : Lucius Hu <lebensterben@users.noreply.github.com>
--
-- Provides utility functions to interact with workspaces.
----------------------------------------------------------------------------------------------------

module Custom.Workspaces
    (
      -- * Workspaces Filter
      WSFilter(..)

      -- * Workspaces Navigation

      -- ** Navigation by Direction
    , moveToWS
    , shiftToWS

      -- ** Navigation by Workspace ID
    , moveToWS'
    , shiftToWS'

      -- * Screen Navigation
    , moveToScreen
    , shiftToScreen

      -- * Util
    , mergeToMaster
    , smartSink
    , smartSinkAll
    ) where

import qualified Data.Map                                as M
import           Data.Maybe                               ( isJust )
import           XMonad                                   ( WorkspaceId
                                                          , X
                                                          , sendMessage
                                                          , whenJust
                                                          , windows
                                                          , withFocused
                                                          , withWindowSet
                                                          )
import           XMonad.Actions.CycleWS                   ( WSType(WSIs)
                                                          , moveTo
                                                          , nextScreen
                                                          , prevScreen
                                                          , shiftNextScreen
                                                          , shiftPrevScreen
                                                          , shiftTo
                                                          )
import           XMonad.Actions.WithAll                   ( sinkAll )
import           XMonad.Config.Prime                      ( Window )
import           XMonad.Layout.SubLayouts                 ( GroupMsg(Merge, UnMergeAll) )
import qualified XMonad.StackSet                         as W
import           XMonad.StackSet                          ( floating )
import           XMonad.Util.Types                        ( Direction1D(..) )

----------------------------------------------------------------------------------------------------
-- Workspaces Filter
----------------------------------------------------------------------------------------------------

-- | Defines what kind of workspaces should be filtered out.
data WSFilter
    = NonScratchPad         -- ^ Filter out \"NamedScratchPad\".
    | NonEmptyScratchPad    -- ^ Filter out \"NamedScratchPad\" and empty workspaces.
    | None                  -- ^ Keep all workspaces.

-- | Given a 'WSFilter', defines what type of workspace should be included in cycle.
filterWS :: WSFilter -> WSType
filterWS fil = case fil of
    None               -> WSIs (return (const True))
    NonScratchPad      -> WSIs (return (\ws -> W.tag ws /= "NSP"))
    NonEmptyScratchPad -> WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "NSP"))

----------------------------------------------------------------------------------------------------
-- Workspaces Navigation
----------------------------------------------------------------------------------------------------

toOrdinal :: Int -> String
toOrdinal i = case last i' of
    '1' -> i' ++ "st"
    '2' -> i' ++ "nd"
    '3' -> i' ++ "rd"
    _   -> i' ++ "th"
    where i' = show i

-- | View the workspace in the given direction with a given filter.
moveToWS :: [(String, Direction1D)] -- ^ Lists of keys-directions pairs, e.g.
                                    --   @[("h", L), ("j", D)]@
         -> WSFilter                -- ^ Filters for candidate workspaces.
         -> [(String, String, X ())]
moveToWS bindings fil = [ (key, show dir, moveTo dir $ filterWS fil) | (key, dir) <- bindings ]

-- | Assign \"Prefix-[1..9]\" keys for switching to i-th workspace.
moveToWS' :: [WorkspaceId] -- ^ Workspaces used in current config
          -> [(String, String, X ())]
moveToWS' wss =
    [ (show index, toOrdinal index, windows $ W.greedyView ws)
    | (ws, index) <- zip wss [1 .. length wss]
    ]

-- | Move the focused window to the workspace in the given direction with a given filter, and keep
-- the window under focus.
shiftToWS :: [(String, Direction1D)] -- ^ Lists of keys-directions pairs, e.g.
                                     --   @[("h", L), ("j", D)]@
          -> WSFilter                -- ^ Filters for candidate workspaces.
          -> [(String, String, X ())]
shiftToWS bindings fil =
    [ (key, show dir, shiftTo dir wsFilter >> moveTo dir wsFilter) | (key, dir) <- bindings ]
    where wsFilter = filterWS fil

-- | Assign \"Prefix-[1..9]\" keys for shifting focused window and follow it to i-th workspace.
shiftToWS' :: [WorkspaceId] -- ^ Workspaces used in current config
           -> [(String, String, X ())]
shiftToWS' wss =
    [ (show index, toOrdinal index, windows (W.shift ws) >> windows (W.greedyView ws))
    | (ws, index) <- zip wss [1 .. length wss]
    ]

----------------------------------------------------------------------------------------------------
-- Screen Navigation
----------------------------------------------------------------------------------------------------

-- | View the screen in the specified direction.
moveToScreen :: [(String, Direction1D)] -- ^ Lists of keys-directions pairs, e.g.
                                        --   @[("h", L), ("j", D)]@
             -> [(String, String, X ())]
moveToScreen bindings =
    [ (key, show dir, if dir == Prev then prevScreen else nextScreen) | (key, dir) <- bindings ]

-- | Move the focused window to the specified direction and keep the window under focus.
shiftToScreen :: [(String, Direction1D)] -- ^ Lists of keys-directions pairs, e.g.
                                         --   @[("h", L), ("j", D)]@
              -> [(String, String, X ())]
shiftToScreen bindings =
    [ ( key
      , show dir
      , if dir == Prev then shiftPrevScreen >> prevScreen else shiftNextScreen >> nextScreen
      )
    | (key, dir) <- bindings
    ]

----------------------------------------------------------------------------------------------------
-- Util
----------------------------------------------------------------------------------------------------

getMaster :: X (Maybe Window)
getMaster = withWindowSet $ \ws -> case W.index ws of
    []      -> return Nothing
    (x : _) -> return $ Just x

-- | Merge focused window to master window of current workspace.
mergeToMaster :: X ()
mergeToMaster = withFocused mergeToMaster'

mergeToMaster' :: Window -> X ()
mergeToMaster' focused = getMaster >>= \case
    Just master | master /= focused -> sendMessage $ Merge focused master
    _                               -> return ()

-- | Unfloat or unmerge the focused window, if any.
-- When the window is floating, it cannot have @Tabbed@ sublayout, and this action unfloat it.
-- Otherwise, try to unmerge the window.
smartSink :: X ()
smartSink = withWindowSet $ \ws -> whenJust (W.peek ws) $ \focused ->
    if M.member focused (floating ws) then windows $ W.sink focused else mergeToMaster' focused

-- | Unfloat or unmerge all windows in current workspace, if any.
-- When current layout is floating, unfloat all windows.
-- Otherwise, try to unmerge all windows.
smartSinkAll :: X ()
smartSinkAll = withWindowSet $ \ws -> whenJust (W.peek ws)
    $ \focused -> if M.null (floating ws) then sendMessage $ UnMergeAll focused else sinkAll
