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
    ) where

import           XMonad                                   ( (<+>)
                                                          , XConfig(startupHook)
                                                          )
import           XMonad.Hooks.SetWMName                   ( setWMName )
import           XMonad.Util.SpawnOnce                    ( spawnOnce )

----------------------------------------------------------------------------------------------------
-- Startup Hook
----------------------------------------------------------------------------------------------------

-- TODO move to Variables module
-- | External programs to launch after xmonad.
startUpApps :: [String]
startUpApps =
    [ "/usr/local/bin/picom --experimental-backend"                 -- compositior
    -- , "/usr/local/bin/mpd --no-daemon"
    , "/usr/bin/mopidy"
    , "/home/lucius/.config/polybar/launch.sh"
    , "/usr/libexec/gsd-xsettings"                                  -- gtk settings daemon
    , "fcitx5"                                                      -- IM daemon
    , "gnome-keyring-daemon --unlock"                               -- prompt for login-password
    , "dropbox start -i"                                            -- dropbox
    , "nitrogen --restore"
    ]

-- | Actions to run when starting up xmonad.
--
-- * Set X Cursor.
-- * Launch external programs. See 'startUpApps'.
-- * Set \"WM\" (window manager) name to \"LG3D\", which supposedly fix a Java GUI program bug.
myStartupHook :: XConfig l -> XConfig l
myStartupHook conf =
    conf { startupHook = setWMName "LG3D" <+> mapM_ spawnOnce startUpApps }
