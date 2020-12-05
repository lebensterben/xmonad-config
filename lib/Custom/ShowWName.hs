{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
----------------------------------------------------------------------------------------------------
-- |
-- Module      : Custom.ShowWName
-- Copyright   : (c) Lucius Hu, 2020
-- License     : BSD3
-- Maintainer  : Lucius Hu <lebensterben@users.noreply.github.com>
--
-- A layout modifier that will show the workspace name, with extra markups stripped.
----------------------------------------------------------------------------------------------------
module Custom.ShowWName
    (
    -- * Show Window Name Layout Modifier
      showWNameEscape
    , ShowWNameEscape
    )
where

import           XMonad                                   ( Window
                                                          , Rectangle(Rectangle)
                                                          , asks
                                                          , fromMessage
                                                          , withWindowSet
                                                          , LayoutMessages(Hide)
                                                          , X
                                                          , XConf(display)
                                                          )
import           XMonad.Hooks.DynamicLog                  ( xmobarStrip )
import           XMonad.Layout.LayoutModifier             ( LayoutModifier(handleMess, redoLayout)
                                                          , ModifiedLayout(ModifiedLayout)
                                                          )
import           XMonad.Layout.ShowWName                  ( SWNConfig(..) )
import qualified XMonad.StackSet                         as W
import           XMonad.Util.Font                         ( fi
                                                          , initXMF
                                                          , releaseXMF
                                                          , textExtentsXMF
                                                          , textWidthXMF
                                                          , Align(AlignCenter)
                                                          )
import           XMonad.Util.Timer                        ( handleTimer
                                                          , startTimer
                                                          , TimerId
                                                          )
import           XMonad.Util.XUtils                       ( createNewWindow
                                                          , deleteWindow
                                                          , paintAndWrite
                                                          , showWindow
                                                          )

----------------------------------------------------------------------------------------------------
-- Show Window Name Layout Modifier
----------------------------------------------------------------------------------------------------

-- | Same as 'XMonad.Layout.ShowWName.showWName\'' except it strip of xmobar markups.
showWNameEscape :: SWNConfig -> l a -> ModifiedLayout ShowWNameEscape l a
showWNameEscape c = ModifiedLayout (SWN True c Nothing)

type ShowWNState = Maybe (TimerId, Window)
data ShowWNameEscape a = SWN Bool SWNConfig ShowWNState deriving (Read, Show)

instance LayoutModifier ShowWNameEscape a where
    redoLayout sn r _ wrs = doShow sn r wrs

    handleMess (SWN _ c (Just (i, w))) m
        | Just e <- fromMessage m = handleTimer i e (deleteWindow w >> return Nothing)
        | Just Hide <- fromMessage m = do
            deleteWindow w
            return . Just $ SWN True c Nothing

    handleMess (SWN _ c s) m | Just Hide <- fromMessage m = return . Just $ SWN True c s
                             | otherwise                  = return Nothing

doShow :: ShowWNameEscape a
       -> Rectangle
       -> [(a, Rectangle)]
       -> X ([(a, Rectangle)], Maybe (ShowWNameEscape a))
doShow (SWN True  c (Just (_, w))) r wrs = deleteWindow w >> flashName c r wrs
doShow (SWN True  c Nothing      ) r wrs = flashName c r wrs
doShow (SWN False _ _            ) _ wrs = return (wrs, Nothing)

flashName :: SWNConfig
          -> Rectangle
          -> [(a, Rectangle)]
          -> X ([(a, Rectangle)], Maybe (ShowWNameEscape a))
flashName c (Rectangle sx sy wh ht) wrs = do
    d        <- asks display
    n        <- xmobarStrip <$> withWindowSet (return . W.currentTag)
    -- NOTE     ^ This is the only change made from "XMonad.Layout.ShowWName"
    f        <- initXMF (swn_font c)
    width    <- (\w -> w + w `div` length n) <$> textWidthXMF d f n
    (as, ds) <- textExtentsXMF f n
    let hight = as + ds
        y     = fi sy + (fi ht - hight + 2) `div` 2
        x     = fi sx + (fi wh - width + 2) `div` 2
    w <- createNewWindow (Rectangle (fi x) (fi y) (fi width) (fi hight)) Nothing "" True
    showWindow w
    paintAndWrite w
                  f
                  (fi width)
                  (fi hight)
                  0
                  (swn_bgcolor c)
                  ""
                  (swn_color c)
                  (swn_bgcolor c)
                  [AlignCenter]
                  [n]
    releaseXMF f
    i <- startTimer (swn_fade c)
    return (wrs, Just $ SWN False c $ Just (i, w))
