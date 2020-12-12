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
      -- * Conversion of 'Runnable' and 'RunnableArgs' to a 'TSNode'.
      AsTSNode(..)
    , AsTSNodeArgs(..)

      -- * Helper functiosn to build the tree select menu.
    , (>*>>)
    , (>:=>)
    , (>$>)
    , tsSubForrest
    , tsSubTree
    , tsSubTitle
    , tsTree
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
import           XMonad.Actions.TreeSelect                ( TSNode(..) )
import           XMonad.Util.PureX                        ( XLike )

----------------------------------------------------------------------------------------------------
-- Conversion of 'Runnable' and 'RunnableArgs' to 'TSNode'
----------------------------------------------------------------------------------------------------

-- | Provides conversion from a 'Runnable' to a 'TSNode' that takes no extra arguments.
class Runnable r => AsTSNode r where
    asTSNode :: (MonadIO m, XLike m) => r -> TSNode (m ())
    asTSNode a = TSNode sdscr ldscr $ appRun a
        where (sdscr, ldscr) = appDscr a

instance AsTSNode App

instance AsTSNode FlatpakApp

instance AsTSNode TerminalApp

instance AsTSNode WebApp

-- | Provides conversion from a 'RunnableArgs' to a 'TSNode' that may take extra arguments.
class RunnableArgs r => AsTSNodeArgs r where
    asTSNodeArgs :: (MonadIO m, XLike m) => r -> [Arg] -> TSNode (m ())
    asTSNodeArgs a args = TSNode sdscr ldscr $ appRunArgs a args
        where (sdscr, ldscr) = appDscr a

instance AsTSNodeArgs App

instance AsTSNodeArgs TerminalApp

----------------------------------------------------------------------------------------------------
-- Helper functions
----------------------------------------------------------------------------------------------------

-- | Composes two 'TSNode' by sequentially composing their actions, and uses the description
-- of the second one.
infixl 4 >*>>
(>*>>) :: TSNode (X ()) -> TSNode (X ()) -> TSNode (X ())
(>*>>) x y = TSNode (tsn_name y) (tsn_extra y) (tsn_value x >> tsn_value y)

-- | Replaces the description of a 'TSNode' by the supplied value, while keep its action intact.
infixl 4 >:=>
(>:=>) :: TSNode (X ()) -> (String, String) -> TSNode (X ())
(>:=>) x (y, z) = TSNode y z (tsn_value x)

-- TODO
infixl 4 >$>
(>$>) :: (a -> X ()) -> TSNode a -> TSNode (X ())
(>$>) x y@TSNode { tsn_value = b } = y { tsn_value = x b }

-- | Constructs a 'Tree' of 'TSNode', with given descriptions, a sub-forest, aand additional
-- 'TSNode'.
--
-- This represents any entry in the tree select menu that at least one of its sub-entry is not a
-- final action, i.e. containing sub-entries itself.
tsSubForrest :: String                 -- ^ Short description.
             -> String                 -- ^ Long description.
             -> Forest (TSNode (X ())) -- ^ A sub-forest that will be attached to the tree.
             -> [TSNode (X ())]        -- ^ Additional 'TSNode's.
             -> Tree (TSNode (X ()))
tsSubForrest fsdscr fldscr tstrees tsnodes =
    Node (tsSubTitle fsdscr fldscr) (tstrees ++ map tsTree tsnodes)

-- | Constructs a 'Tree' of 'TSNode', with given descriptions, and a number of 'TSNode'.
--
-- This represent an entry in tree select menu, s.t. all of its sub-entries are final actions.
tsSubTree :: String -> String -> [TSNode (X ())] -> Tree (TSNode (X ()))
tsSubTree tsdscr tldscr tsnodes = Node (tsSubTitle tsdscr tldscr) (map tsTree tsnodes)

-- TODO
tsSubTitle :: (Monad m) => String -> String -> TSNode (m ())
tsSubTitle sdscr ldscr = TSNode sdscr ldscr (return ())

-- TODO
tsTree :: TSNode (m a) -> Tree (TSNode (m a))
tsTree = flip Node []

-- | A separator
tsSeparator :: Int -> Tree (TSNode (X ()))
tsSeparator n = Node (tsSubTitle (replicate n '-') "") []
