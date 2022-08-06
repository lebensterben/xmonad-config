{-# LANGUAGE LambdaCase #-}
----------------------------------------------------------------------------------------------------
-- |
-- Module      : Custom.Workspaces
-- Copyright   : (c) Lucius Hu, 2020-2022
-- License     : BSD3
-- Maintainer  : Lucius Hu <lebensterben@users.noreply.github.com>
--
-- Provides utility functions to interact with workspaces.
----------------------------------------------------------------------------------------------------

module Custom.Util.Workspaces
    ( moveToWS
    , shiftToWS
    , moveToWS'
    , shiftToWS'
    , moveToScreen
    , shiftToScreen
    , shrinkFrom
    , mergeToMaster
    , smartSink
    , smartSinkAll
    ) where

import qualified Data.Map                                as M
import           Graphics.X11                             ( Window )
import           XMonad                                   ( Resize(..)
                                                          , WorkspaceId
                                                          , X
                                                          , description
                                                          , io
                                                          , sendMessage
                                                          , spawn
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
import           XMonad.Layout.BinarySpacePartition       ( ResizeDirectional(..) )
import           XMonad.Layout.ResizableTile              ( MirrorResize(..) )
import           XMonad.Layout.SubLayouts                 ( GroupMsg(..) )
import qualified XMonad.StackSet                         as W
import           XMonad.StackSet                          ( current
                                                          , floating
                                                          , layout
                                                          , workspace
                                                          )
import           XMonad.Util.Types                        ( Direction1D(..)
                                                          , Direction2D(..)
                                                          )

----------------------------------------------------------------------------------------------------
-- Workspaces Navigation
----------------------------------------------------------------------------------------------------

-- | Convert numerical integers to strings as oridinal numbers.
toOrdinal :: Int -> String
toOrdinal i = case last i' of
    '1' -> i' ++ "st"
    '2' -> i' ++ "nd"
    '3' -> i' ++ "rd"
    _   -> i' ++ "th"
    where i' = show i

-- | Human readable descrption of 'Direction1D'.
descDir1D :: Direction1D -> String
descDir1D = \case
    Prev -> "last"
    Next -> "next"

-- | View the workspace in the given direction with a given filter.
moveToWS :: [(String, Direction1D)] -- ^ Lists of keys-directions pairs, e.g.
                                    --   @[("h", L), ("j", D)]@
         -> [(String, String, X ())]
moveToWS bindings = [ (key, descDir1D dir, moveTo dir wsFilter) | (key, dir) <- bindings ]
    where wsFilter = WSIs (return (\ws -> W.tag ws /= "NSP"))

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
          -> [(String, String, X ())]
shiftToWS bindings =
    [ (key, descDir1D dir, shiftTo dir wsFilter >> moveTo dir wsFilter) | (key, dir) <- bindings ]
    where wsFilter = WSIs (return (\ws -> W.tag ws /= "NSP"))

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
    [ (key, descDir1D dir, if dir == Prev then prevScreen else nextScreen)
    | (key, dir) <- bindings
    ]

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
-- Window Resizing
----------------------------------------------------------------------------------------------------

-- | Resize windows by shrinking windows on the specified direction.
shrinkFrom :: Direction2D -> X ()
shrinkFrom dir = withWindowSet $ \ws -> case description . layout . workspace $ current ws of
    "Binary" -> sendMessage $ case dir of
      -- These looks arbitrary, but not. See usage of "XMonad.Layout.BinarySpacePartition".
        L -> ExpandTowards L
        R -> ShrinkFrom L
        D -> ShrinkFrom U
        U -> ExpandTowards U
    "Tall" -> case dir of
        L -> sendMessage Shrink
        R -> sendMessage Expand
        D -> sendMessage MirrorShrink
        U -> sendMessage MirrorExpand
    l -> io $ spawn $ "dunstify '" ++ l ++ " layout does not support resizing.'"

----------------------------------------------------------------------------------------------------
-- Util
----------------------------------------------------------------------------------------------------

-- | Returns the master window of current workspace, if there's any.
getMaster :: X (Maybe Window)
getMaster = withWindowSet $ \ws -> case W.index ws of
    []      -> return Nothing
    (x : _) -> return $ Just x

-- | Merge focused window to master window of current workspace.
mergeToMaster :: X ()
mergeToMaster = withFocused mergeToMaster'

-- | Merge the given window to master window of current workspace.
-- Do nothing if there's no master window.
mergeToMaster' :: Window -- ^ Window to be merged.
               -> X ()
mergeToMaster' focused = getMaster >>= \case
    Just master | master /= focused -> sendMessage $ Merge focused master
    _                               -> return ()

-- | Unfloat or unmerge the focused window, if any.
-- When the window is floating, it cannot have @Tabbed@ sublayout, and this action unfloat it.
-- Otherwise, try to unmerge the window.
smartSink :: X ()
smartSink = withWindowSet $ \ws -> whenJust (W.peek ws) $ \focused ->
    if M.member focused (floating ws)
        then windows $ W.sink focused
        else sendMessage $ UnMerge focused

-- | Unfloat or unmerge all windows in current workspace, if any.
--
-- When current layout is floating, unfloat all windows.
-- Otherwise, try to unmerge all windows.
smartSinkAll :: X ()
smartSinkAll = withWindowSet $ \ws -> whenJust (W.peek ws)
    $ \focused -> if M.null (floating ws) then sendMessage $ UnMergeAll focused else sinkAll
