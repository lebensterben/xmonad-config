----------------------------------------------------------------------------------------------------
-- |
-- Module      : Custom.Util.Run
-- Copyright   : (c) Lucius Hu, 2022-2022
-- License     : BSD3
-- Maintainer  : Lucius Hu <lebensterben@users.noreply.github.com>
--
-- Override symbos in 'XMonad.Util.Run'.
----------------------------------------------------------------------------------------------------
module Custom.Util.Run where
import           XMonad                                   ( X )
import           XMonad.Util.Run                          ( (>->)
                                                          , Input
                                                          )

(>-$) :: X Input -> String -> X Input
(>-$) xi xs = xi >-> pure (mkDList xs)
  where
    mkDList :: String -> ShowS
    mkDList = (<>) . (<> " ")
infixr 3 >-$

(>-$@) :: X Input -> [String] -> X Input
(>-$@) xi xs = xi >-> pure (mkDList xs)
  where
    mkDList :: [String] -> ShowS
    mkDList a = (<>) (unwords a <> " ")
infixr 3 >-$@