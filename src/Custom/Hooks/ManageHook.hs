----------------------------------------------------------------------------------------------------
-- |
-- Module      : Custom.Hooks.ManageHook
-- Copyright   : (c) Lucius Hu, 2020-2022
-- License     : BSD3
-- Maintainer  : Lucius Hu <lebensterben@users.noreply.github.com>
--
-- Defines actions to run when a new window is opened.
----------------------------------------------------------------------------------------------------

module Custom.Hooks.ManageHook (myManageHook) where

import           Control.Applicative                      ( (<|>) )
import           Control.Monad                            ( liftM2 )
import           Custom.Variables                         ( centerFloatApps
                                                          , centerFloatClasses
                                                          , myScratchPads
                                                          , shiftWSApps
                                                          , shiftWSClassses
                                                          )
import           Data.Function                            ( (&) )
import qualified Data.HashMap.Strict                     as M
import qualified Data.HashSet                            as S
import           XMonad                                   ( (<||>)
                                                          , ManageHook
                                                          , appName
                                                          , className
                                                          , composeAll
                                                          , doShift
                                                          )
import           XMonad.Hooks.ManageHelpers               ( (-?>)
                                                          , (-?>>)
                                                          , (</=?)
                                                          , composeOne
                                                          , doCenterFloat
                                                          , doFullFloat
                                                          , isDialog
                                                          , isFullscreen
                                                          , transience
                                                          )
import           XMonad.Util.NamedScratchpad              ( namedScratchpadManageHook )
import           XMonad.Util.PureX                        ( whenJust' )

----------------------------------------------------------------------------------------------------
-- Mangage Hook
----------------------------------------------------------------------------------------------------

-- | Actions to run when a new window is opened.
--
-- Actions falls into three groups:
--
-- * Named Scratch Pad:
--
--     * If the new window belongs to one of 'myScratchPads',
--     run its associated action.
--
-- * Movement:
--
--     * If the new window is transient, send it to its parent window.
--     * If the 'className' or 'appName' matches one of the defined rules,
--     move it to the specified workspace (if it's currently present).
--
-- * Geometry:
--
--     * If the new window wants to fill the fullscreen, makes it a full float window.
--     * If the new window is a dialog, or if its 'className' or 'appName' matches
--     one of the define rules, make it a centered float window.
myManageHook :: ManageHook
myManageHook = composeAll
    [ namedScratchpadManageHook myScratchPads
    , composeOne
        [ isFullscreen -?> doFullFloat
        , isDialog
        <||> memberM appName   centerFloatApps
        <||> memberM className centerFloatClasses
        -?>  doCenterFloat
        ]
    , composeOne
        [ transience
        , liftM2 (<|>) (lookupM className shiftWSClassses) (lookupM appName shiftWSApps)
        & (</=? Nothing)
        & (-?>> (`whenJust'` doShift))
        ]
    ]
  where
    memberM x xs = (`S.member` xs) <$> x
    lookupM x xs = (`M.lookup` xs) <$> x
