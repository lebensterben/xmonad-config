{-# LANGUAGE FlexibleContexts #-}
module Custom.Util.Polybar
    ( polybar
    , polybarFgColor
    , polybarBgColor
    , polybarFont
    , polybarAction
    , polybarUnderline
    ) where

import           Custom.Util.Color                        ( HexColor )
import           Custom.Util.DBus                         ( dbusOutput )
import qualified DBus.Client                             as D
import           XMonad                                   ( (<+>)
                                                          , LayoutClass
                                                          , Window
                                                          , XConfig(layoutHook, logHook)
                                                          )
import           XMonad.Hooks.DynamicLog                  ( PP(..)
                                                          , dynamicLogWithPP
                                                          , wrap
                                                          )
import           XMonad.Hooks.ManageDocks                 ( AvoidStruts
                                                          , avoidStruts
                                                          , docks
                                                          )
import           XMonad.Layout.LayoutModifier             ( ModifiedLayout )

-- TODO: Add DOCS
polybar :: LayoutClass l Window
        => D.Client
        -> PP
        -> XConfig l
        -> XConfig (ModifiedLayout AvoidStruts l)
polybar dbus pp conf = docks $ conf
    { layoutHook = avoidStruts (layoutHook conf)
    , logHook    = dynamicLogWithPP pp { ppOutput = dbusOutput dbus } <+> logHook conf
    }

polybarFgColor :: HexColor -- ^ Color.
               -> String   -- ^ String to format.
               -> String
polybarFgColor c = flip wrap "%{F-}" $ "%{F" ++ c ++ "}"

polybarBgColor :: HexColor -- ^ Color.
               -> String   -- ^ String to format.
               -> String
polybarBgColor c = flip wrap "%{B-}" $ "%{B" ++ c ++ "}"

--TODO: Unused
polybarFont :: Integer -- ^ Index of the font as defined in Polybar's config file.
            -> String  -- ^ String to format.
            -> String
polybarFont i = flip wrap "%{T-}" $ "%{T" ++ show i ++ "}"

polybarAction :: Integer -- ^ Mouse button.
                         -- 1 to 5 are same as X11 Mouse Button.
                         -- 6 to 8 are double click of 1 to 3.
              -> String  -- ^ Action.
              -> String  -- ^ String to format.
              -> String
polybarAction i a = flip wrap "%{A}" $ "%{A" ++ show i ++ ":" ++ a ++ ":}"

polybarUnderline :: HexColor -- ^ Color.
                 -> String   -- ^ String to format.
                 -> String
polybarUnderline c = flip wrap "%{-u}" $ "%{u" ++ c ++ "}%{+u}"
