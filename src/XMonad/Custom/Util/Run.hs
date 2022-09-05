----------------------------------------------------------------------------------------------------
-- |
-- Module      : XMonad.Custom.Util.Run
-- Copyright   : (c) Lucius Hu, 2022
-- License     : BSD3
-- Maintainer  : Lucius Hu <lebensterben@users.noreply.github.com>
--
-- Override symbos in 'XMonad.Util.Run'.
----------------------------------------------------------------------------------------------------
module XMonad.Custom.Util.Run where

import           XMonad                                   ( X )
import           XMonad.Util.Run                          ( (>->)
                                                          , Input
                                                          )

(>-$@) :: X Input -> X [String] -> X Input
(>-$@) xi xs = xi >-> fmap mkDList xs
  where
    mkDList :: [String] -> ShowS
    mkDList a = (<>) (unwords a <> " ")
infixr 3 >-$@
