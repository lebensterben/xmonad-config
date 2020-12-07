{-# LANGUAGE DeriveDataTypeable #-}
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
    , wsFind
    , Workspaces(..)
    , WorkspacesStorage(..)

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
import           Data.List                                ( elemIndex )
import           XMonad                                   ( ExtensionClass(..)
                                                          , Typeable
                                                          , WorkspaceId
                                                          , withWindowSet
                                                          , windows
                                                          , X
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
import qualified XMonad.Util.ExtensibleState             as XS
import           XMonad.Util.Types                        ( Direction1D(..) )

----------------------------------------------------------------------------------------------------
-- Workspaces
----------------------------------------------------------------------------------------------------

-- Currently only store wsLbls, and don't support dynamic workspaces yet
-- | A storage for workspace raw labels.
--
-- Needs to be initialised during starting up, via 'XMonad.Util.ExtensibleState.put', and can be
-- retrieved via 'XMonad.Util.ExtensibleState.get'.
newtype WorkspacesStorage = WorkspacesStorage [WorkspaceId] deriving Typeable
instance ExtensionClass WorkspacesStorage where
    initialValue = WorkspacesStorage []

-- | Represents the set of workspaces in XMonad.
--
-- >>> myWorkspacesLbl = ["1", "2", "3"]
-- >>> Workspaces { wsLbls = myWorkspacesLbl, formatter = /omitted/ }
-- Workspaces { wsLbls = ["1","2","3"] }
data Workspaces = Workspaces {
    wsLbls :: [WorkspaceId], -- ^ Labels of workspaces.
    formatter :: (Int, WorkspaceId) -> WorkspaceId
                             -- ^ Function that formats the raw labels, e.g. adding extra
                             --   spacings, and adding XMobar action tag.
    }

instance Show Workspaces where
    show =
        \wss -> "Workspaces { wsLbls = " ++ show (wsLbls wss) ++ ", formatter = /omitted/" ++ " }"

-- | Retrieve the list of workspaces labels, formatted with the formatter, and add xmobar actions
--   according to whether it's clickable.
wsLabels :: Workspaces -> [WorkspaceId]
wsLabels wss = zipWith (curry $ formatter wss) [1 .. length lbls] lbls where lbls = wsLbls wss

-- | Given a workspace tag, find it in current workspaces, and reformat it appropriately.
--   If it's not found, return @Nothing@.
wsFind :: WorkspaceId -> X (Maybe WorkspaceId)
wsFind x = do
    (WorkspacesStorage wss) <- XS.get
    withWindowSet $ \ws -> do
        let curWSs = W.tag <$> W.workspaces ws
        return $ (curWSs !!) <$> elemIndex x wss

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
         -> [(String, Direction1D, X ())]
moveToWS prefix bindings fil =
    [ (prefix ++ key, dir, moveTo dir $ filterWS fil) | (key, dir) <- bindings ]

-- | Assign \"Prefix-[1..9]\" keys for switching to i-th workspace.
moveToWS' :: Workspaces -- ^ Workspaces used in current config
          -> String     -- ^ Prefix keys, ends with a \"-\", e.g. @"M-C-"@
          -> [(String, Int, X ())]
moveToWS' wss prefix =
    [ (prefix ++ show index, index, windows $ W.greedyView ws)
    | let wsl = wsLabels wss
    , (ws, index) <- zip wsl [1 .. length wsl]
    ]

-- | Move the focused window to the workspace in the given direction with a given filter, and keep
--   the window under focus.
shiftToWS :: String                  -- ^ Prefix keys, ends with a \"-\", e.g. @"M-C-"@
          -> [(String, Direction1D)] -- ^ Lists of keys-directions pairs, e.g.
                                     --   @[("h", L), ("j", D)]@
          -> WSFilter                -- ^ Filters for candidate workspaces.
          -> [(String, Direction1D, X ())]
shiftToWS prefix bindings fil =
    [ (prefix ++ key, dir, shiftTo dir wsFilter >> moveTo dir wsFilter) | (key, dir) <- bindings ]
    where wsFilter = filterWS fil

-- | Assign \"Prefix-[1..9]\" keys for shifting focused window and follow it to i-th workspace.
shiftToWS' :: Workspaces -- ^ Workspaces used in current config
           -> String     -- ^ Prefix keys, ends with a \"-\", e.g. @"M-C-"@
           -> [(String, Int, X ())]
shiftToWS' wss prefix =
    [ (prefix ++ show index, index, windows (W.shift ws) >> windows (W.greedyView ws))
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
             -> [(String, Direction1D, X ())]
moveToScreen prefix bindings =
    [ (prefix ++ key, dir, if dir == Prev then prevScreen else nextScreen)
    | (key, dir) <- bindings
    ]

-- | Move the focused window to the specified direction and keep the window under focus.
shiftToScreen :: String                  -- ^ Prefix keys, ends with a \"-\", e.g. @"M-C-"@
              -> [(String, Direction1D)] -- ^ Lists of keys-directions pairs, e.g.
                                         --   @[("h", L), ("j", D)]@
              -> [(String, Direction1D, X ())]
shiftToScreen prefix bindings =
    [ ( prefix ++ key
      , dir
      , if dir == Prev then shiftPrevScreen >> prevScreen else shiftNextScreen >> nextScreen
      )
    | (key, dir) <- bindings
    ]
