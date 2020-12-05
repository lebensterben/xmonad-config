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
      -- * Workspaces
      wsLabels
    , Workspaces(..)

      -- * Workspaces Filter
    , WSFilter(..)

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
    )
where

import           Data.Maybe                               ( isJust )
import           Custom.Util.XMobar                       ( xmobarEscape )
import           XMonad                                   ( X
                                                          , windows
                                                          )
import           XMonad.Actions.CycleWS                   ( moveTo
                                                          , nextScreen
                                                          , prevScreen
                                                          , shiftNextScreen
                                                          , shiftPrevScreen
                                                          , shiftTo
                                                          , WSType(WSIs)
                                                          )
import qualified XMonad.StackSet                         as W
import           XMonad.Util.Types                        ( Direction1D(..) )
import           XMonad.Hooks.DynamicLog                  ( xmobarAction )

----------------------------------------------------------------------------------------------------
-- Workspaces
----------------------------------------------------------------------------------------------------

-- | Represents the set of workspaces in XMonad.
--
-- >>> myWorkspacesLbl = ["1", "2", "3"]
-- >>> Workspaces { wsLbls = myWorkspacesLbl, clickable = True }
-- Workspaces {wsLbls = ["1","2","3"], clickable = True}
data Workspaces = Workspaces {
    wsLbls :: [String], -- ^ Labels of workspaces
    clickable :: Bool   -- ^ Whether it's clickable
    } deriving (Show)

-- Retrieve the list of workspaces labels, formatted according to whether it's clickable.
wsLabels :: Workspaces -> [String]
wsLabels ws = if clickable ws then lbls $ map xmobarEscape $ wsLbls ws else wsLbls ws
  where
    lbls :: [String] -> [String]
    lbls l =
        [ xmobarAction ("xdotool key super+" ++ show i) "1" n
        | (i, n) <- zip [1 :: Integer .. 9] l
        ]

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
    NonScratchPad      -> WSIs (return (\ws -> W.tag ws /= "nsp"))
    NonEmptyScratchPad -> WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "nsp"))

----------------------------------------------------------------------------------------------------
-- Workspaces Navigation
----------------------------------------------------------------------------------------------------

-- | View the workspace in the given direction with a given filter.
moveToWS :: String                  -- ^ Prefix keys, ends with a \"-\", e.g. @"M-C-"@
         -> [(String, Direction1D)] -- ^ Lists of keys-directions pairs, e.g.
                                    --   @[("h", L), ("j", D)]@
         -> WSFilter                -- ^ Filters for candidate workspaces.
         -> [(String, X ())]
moveToWS prefix bindings fil =
    [ (prefix ++ key, moveTo dir $ filterWS fil) | (key, dir) <- bindings ]

-- | Assign \"Prefix-[1..9]\" keys for switching to i-th workspace.
moveToWS' :: Workspaces              -- ^ Workspaces used in current config
          -> String                  -- ^ Prefix keys, ends with a \"-\", e.g. @"M-C-"@
          -> [(String, X ())]
moveToWS' wss prefix =
    [ (prefix ++ show index, windows $ W.greedyView ws)
    | let wsl = wsLabels wss
    , (ws, index) <- zip wsl [1 .. length wsl]
    ]

-- | Move the focused window to the workspace in the given direction with a given filter, and keep
--   the window under focus.
shiftToWS :: String                  -- ^ Prefix keys, ends with a \"-\", e.g. @"M-C-"@
          -> [(String, Direction1D)] -- ^ Lists of keys-directions pairs, e.g.
                                     --   @[("h", L), ("j", D)]@
          -> WSFilter                -- ^ Filters for candidate workspaces.
          -> [(String, X ())]
shiftToWS prefix bindings fil =
    [ (prefix ++ key, shiftTo dir wsFilter >> moveTo dir wsFilter) | (key, dir) <- bindings ]
    where wsFilter = filterWS fil

-- | Assign \"Prefix-[1..9]\" keys for shifting focused window and follow it to i-th workspace.
shiftToWS' :: Workspaces              -- ^ Workspaces used in current config
           -> String                  -- ^ Prefix keys, ends with a \"-\", e.g. @"M-C-"@
           -> [(String, X ())]
shiftToWS' wss prefix =
    [ (prefix ++ show index, windows (W.shift ws) >> windows (W.greedyView ws))
    | let wsl = wsLabels wss
    , (ws, index) <- zip wsl [1 .. length wsl]
    ]

----------------------------------------------------------------------------------------------------
-- Screen Navigation
----------------------------------------------------------------------------------------------------

-- | View the screen in the specified direction.
moveToScreen :: String                  -- ^ Prefix keys, ends with a \"-\", e.g. @"M-C-"@
             -> [(String, Direction1D)] -- ^ Lists of keys-directions pairs, e.g.
                                        --   @[("h", L), ("j", D)]@
             -> [(String, X ())]
moveToScreen prefix bindings =
    [ (prefix ++ key, if dir == Prev then prevScreen else nextScreen) | (key, dir) <- bindings ]

-- | Move the focused window to the specified direction and keep the window under focus.
shiftToScreen :: String                  -- ^ Prefix keys, ends with a \"-\", e.g. @"M-C-"@
              -> [(String, Direction1D)] -- ^ Lists of keys-directions pairs, e.g.
                                         --   @[("h", L), ("j", D)]@
              -> [(String, X ())]
shiftToScreen prefix bindings =
    [ ( prefix ++ key
      , if dir == Prev then shiftPrevScreen >> prevScreen else shiftNextScreen >> nextScreen
      )
    | (key, dir) <- bindings
    ]
