----------------------------------------------------------------------------------------------------
-- |
-- Module      : Custom.Hooks.StartupHook
-- Copyright   : (c) Lucius Hu, 2020
-- License     : BSD3
-- Maintainer  : Lucius Hu <lebensterben@users.noreply.github.com>
--
-- Defines actions to run after xmonad starts up.
----------------------------------------------------------------------------------------------------
module Custom.Hooks.StartupHook
    (
    -- * Startup Hook
      myStartupHook
    )
where

import           XMonad                                   ( xC_left_ptr
                                                          , XConfig(startupHook)
                                                          )
import           XMonad.Hooks.SetWMName                   ( setWMName )
import           XMonad.Util.Cursor                       ( setDefaultCursor )
import           XMonad.Util.SpawnOnce                    ( spawnOnce )

----------------------------------------------------------------------------------------------------
-- Startup Hook
----------------------------------------------------------------------------------------------------

-- | External programs to launch after xmonad.
startUpApps :: [String]
startUpApps =
    [ "/usr/local/bin/picom"                                      -- compositior
    , "ulauncher --hide-window"                                   -- app launcher
    , "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1" -- polkit daemon
    , "gnome-keyring-daemon -r"                                   -- keyring daemon
    , "/usr/libexec/gsd-xsettings"                                -- gtk settings daemon
    , "nm-applet"                                                 -- network tray icon
    , "/usr/local/bin/volumeicon"                                 -- volume tray icon
    , "/usr/local/bin/trayer --edge top --align right --widthtype request --padding 6 \
        \ --SetDockType true --SetPartialStrut true --expand true --monitor 1 --transparent true \
        \ --alpha 0 --tint 0x282c34  --height 22"                 -- trayer
    , "fcitx5"                                                    -- IM daemon
    ]

-- | Actions to run when starting up xmonad.
--
-- * Set X Cursor.
-- * Launch external programs. See 'startUpApps'.
-- * Set \"WM\" (window manager) name to \"LG3D\", which supposedly fix a Java GUI program bug.
myStartupHook :: XConfig l -> XConfig l
myStartupHook conf = conf
    { startupHook = setDefaultCursor xC_left_ptr >> mapM_ spawnOnce startUpApps >> setWMName "LG3D"
    }
