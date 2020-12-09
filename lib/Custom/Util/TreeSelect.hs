----------------------------------------------------------------------------------------------------
-- |
-- Module      : Custom.Util.TreeSelect
-- Copyright   : (c) Lucius Hu, 2020
-- License     : BSD3
-- Maintainer  : Lucius Hu <lebensterben@users.noreply.github.com>
--
-- Helper functions to build a tree select menu.
----------------------------------------------------------------------------------------------------

module Custom.Util.TreeSelect
    (
      -- * Conversion of 'Runnable' and 'RunnableArgs' to a 'TS.TSNode'.
      AsTSNode(..)
    , AsTSNodeArgs(..)

      -- * Helper functiosn to build the tree select menu.
    , (>+>)
    , (>:=>)
    , tsSubForrest
    , tsSubTree
    , tsSeparator
    )
where

import           Custom.Util.Apps                         ( App
                                                          , Arg
                                                          , FlatpakApp
                                                          , Runnable(..)
                                                          , RunnableArgs(..)
                                                          , TerminalApp
                                                          , WebApp
                                                          )
import           Data.Tree                                ( Forest
                                                          , Tree(..)
                                                          )
import           XMonad                                   ( MonadIO
                                                          , X
                                                          )
import qualified XMonad.Actions.TreeSelect               as TS
import           XMonad.Util.PureX                        ( XLike )

----------------------------------------------------------------------------------------------------
-- Conversion of 'Runnable' and 'RunnableArgs' to 'TS.TSNode'
----------------------------------------------------------------------------------------------------

-- | Provides conversion from a 'Runnable' to a 'TS.TSNode' that takes no extra arguments.
class Runnable r => AsTSNode r where
    asTSNode :: (MonadIO m, XLike m) => r -> TS.TSNode (m ())
    asTSNode a = TS.TSNode sdscr ldscr $ appRun a
        where (sdscr, ldscr) = appDscr a

instance AsTSNode App

instance AsTSNode FlatpakApp

instance AsTSNode TerminalApp

instance AsTSNode WebApp

-- | Provides conversion from a 'RunnableArgs' to a 'TS.TSNode' that may take extra arguments.
class RunnableArgs r => AsTSNodeArgs r where
    asTSNodeArgs :: (MonadIO m, XLike m) => r -> [Arg] -> TS.TSNode (m ())
    asTSNodeArgs a args = TS.TSNode sdscr ldscr $ appRunArgs a args
        where (sdscr, ldscr) = appDscr a

instance AsTSNodeArgs App

instance AsTSNodeArgs TerminalApp

----------------------------------------------------------------------------------------------------
-- Helper functions
----------------------------------------------------------------------------------------------------

-- | Composes two 'TS.TSNode' by sequentially composing their actions, and uses the description
-- of the second one.
infixl 4 >+>
(>+>) :: Monad m => TS.TSNode (m a) -> TS.TSNode (m b) -> TS.TSNode (m b)
(>+>) x y = TS.TSNode (TS.tsn_name y) (TS.tsn_extra y) (TS.tsn_value x >> TS.tsn_value y)

-- | Replaces the description of a 'TS.TSNode' by the supplied value, while keep its action intact.
infixl 4 >:=>
(>:=>) :: Monad m => TS.TSNode (m a) -> (String, String) -> TS.TSNode (m a)
(>:=>) x (y, z) = TS.TSNode y z (TS.tsn_value x)

-- | Constructs a 'Tree' of 'TS.TSNode', with given descriptions, a sub-forest, aand additional
-- 'TS.TSNode'.
--
-- This represents any entry in the tree select menu that at least one of its sub-entry is not a
-- final action, i.e. containing sub-entries itself.
tsSubForrest :: String                    -- ^ Short description.
             -> String                    -- ^ Long description.
             -> Forest (TS.TSNode (X ())) -- ^ A sub-forest that will be attached to the tree.
             -> [TS.TSNode (X ())]        -- ^ Additional 'TS.TSNode's.
             -> Tree (TS.TSNode (X ()))
tsSubForrest fsdscr fldscr tstrees tsnodes =
    Node (tsSubTitle fsdscr fldscr) (tstrees ++ map (`Node` []) tsnodes)

-- | Constructs a 'Tree' of 'TS.TSNode', with given descriptions, and a number of 'TS.TSNode'.
--
-- This represent an entry in tree select menu, s.t. all of its sub-entries are final actions.
tsSubTree :: String -> String -> [TS.TSNode (X ())] -> Tree (TS.TSNode (X ()))
tsSubTree tsdscr tldscr tsnodes = Node (tsSubTitle tsdscr tldscr) (map (`Node` []) tsnodes)

tsSubTitle :: String -> String -> TS.TSNode (X ())
tsSubTitle sdscr ldscr = TS.TSNode sdscr ldscr (return ())

-- | A separator
tsSeparator :: Int -> Tree (TS.TSNode (X ()))
tsSeparator n = Node (tsSubTitle (replicate n '-') "") []
