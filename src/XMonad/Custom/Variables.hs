----------------------------------------------------------------------------------------------------
-- |
-- Module      : XMonad.Custom.Variables
-- Copyright   : (c) Lucius Hu, 2020-2022
-- License     : BSD3
-- Maintainer  : Lucius Hu <lebensterben@users.noreply.github.com>
--
-- Defines top level customizaton variables.
----------------------------------------------------------------------------------------------------

module XMonad.Custom.Variables where

import qualified Data.HashMap.Strict                     as M
import qualified Data.HashSet                            as S
import           XMonad                                   ( (=?)
                                                          , Dimension
                                                          , WorkspaceId
                                                          , className
                                                          , def
                                                          )
import           XMonad.Layout.Decoration                 ( Theme(..) )
import           XMonad.Layout.ShowWName                  ( SWNConfig(..) )
import qualified XMonad.StackSet                         as S
import           XMonad.Util.NamedScratchpad              ( NamedScratchpad(NS)
                                                          , NamedScratchpads
                                                          , customFloating
                                                          )
import           XMonad.Util.Run                          ( ProcessConfig(..) )

----------------------------------------------------------------------------------------------------
-- Process Config
----------------------------------------------------------------------------------------------------
myProcessConfig :: ProcessConfig
myProcessConfig = def { emacsLispDir = "~/.emacs.d/", emacsElpaDir = "~/.emacs.d/elpa/" }

----------------------------------------------------------------------------------------------------
-- Color Scheme And Layout
----------------------------------------------------------------------------------------------------
colorBg, colorBgAlt, base6, base8, colorFg, orange :: String
colorBg = "#282c34"
colorBgAlt = "#21242b"
-- base0 = "#1b2229"
-- base1 = "#1c1f24" -- black
-- base2 = "#23272e" -- base3
-- base3 = "#32363e" -- bg lighten 0.05
-- base4 = "#3f444a" -- grey
-- base5 = "#42444a" -- bgAlt lighten 0.15
base6 = "#5b6268" -- base5
-- base7 = "#6b6d70" -- black lighten 0.35
base8 = "#dfdfdf"
colorFg = "#bbc2cf"
-- colorFgAlt = "#5b6268" -- also base 5
-- red = "#ff6c6b"
-- orange = "color9"
orange = "#da8548"
-- green = "#98be65"
-- teal = "#4db5bd" -- unused
-- yellow = "#ccbe7b"
-- blue = "#51afef"
-- dimBlue = "#2c6083" -- blue darken 0.45
-- darkBlue = "#2257a0" -- bg highlight
-- magenta = "#c678dd"
-- brightMagenta = "#dcaeea" -- magenta lighten 0.4
-- violet = "#a9a1e1"
-- cyan = "#46d9ff"
-- darkCyan = "#5699af"
-- dimCyan = "#80b2c3" -- darkCyan lighten 0.25

myBorderWidth :: Dimension
myBorderWidth = 0

mySpacingWidth :: Integer
mySpacingWidth = 4

----------------------------------------------------------------------------------------------------
-- Tabbed Sublayout Config
----------------------------------------------------------------------------------------------------

-- | Config for Tabbed (sub-)layout.
tabbedConfig :: Theme
tabbedConfig = def { activeColor         = colorBg     -- #282c34
                   , inactiveColor       = base6       -- #5b6268
                   , urgentColor         = orange      -- #da8548
                   , activeBorderWidth   = myBorderWidth
                   , inactiveBorderWidth = myBorderWidth
                   , urgentBorderWidth   = myBorderWidth
                   , activeTextColor     = base8       -- #dfdfdf
                   , inactiveTextColor   = colorFg     -- #bbc2cf
                   , urgentTextColor     = base8       -- #dfdfdf
                   , fontName            = "xft:Arimo-12.0:normal"
                   }

----------------------------------------------------------------------------------------------------
-- Workspaces
----------------------------------------------------------------------------------------------------

myWorkspaces :: [WorkspaceId]
myWorkspaces = ["dev", "www", "sys", "irc", "mus"]

----------------------------------------------------------------------------------------------------
-- ShowWName Config
----------------------------------------------------------------------------------------------------

-- | Config for showing workspaces name.
showWNameConfig :: SWNConfig
showWNameConfig = def { swn_font    = "xft:Arimo-60.0:bold"
                      , swn_fade    = 1.0
                      , swn_bgcolor = colorBgAlt -- #1c1f24
                      , swn_color   = base8      -- #dfdfdf
                      }

----------------------------------------------------------------------------------------------------
-- Named Scracthpads
----------------------------------------------------------------------------------------------------

myScratchPads :: NamedScratchpads
myScratchPads =
    [ NS "terminal"
         "alacritty --class 'AlacrittyNSP,AlacrittyNSP'"
         (className =? "AlacrittyNSP")
         nspFloating
    , NS "zeal" "zeal" (className =? "Zeal") nspFloating
    , NS "quickDocs"
         "com.github.mdh34.quickdocs"
         (className =? "Com.github.mdh34.quickdocs")
         nspFloating
    ]
    where nspFloating = customFloating (S.RationalRect 0.2 0.2 0.6 0.6) -- left top width height

----------------------------------------------------------------------------------------------------
-- Startup Apps
----------------------------------------------------------------------------------------------------

-- | External programs to launch after xmonad.
startUpApps :: [String]
startUpApps =
    [ "/home/lucius/.local/bin/status-notifier-watcher"
    , "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1"
    -- , "systemctl --user start emacs.service"
    , "picom --experimental-backend"
    , "dex -a -s /home/lucius/.config/autostart/"
    -- , "/home/lucius/.config/polybar/launch.sh"
    , "taffybar"
    -- , "mopidy"
    , "volctl"
    , "nm-applet"
    , "1password --silent"
    -- , "nitrogen --restore"
    ]

----------------------------------------------------------------------------------------------------
-- Matching rules for manage hook.
--
-- Below \"app name\" and \"class name\" corresponds to the two fields of \"WM_CLASS\" of X.
----------------------------------------------------------------------------------------------------

-- | Mapping of class name to workspace name where they should be shift to.
shiftWSClassses :: M.HashMap String WorkspaceId
shiftWSClassses = M.fromList
    [("firefox", "www"), ("Element", "irc"), ("Gitter", "irc"), ("Hexchat", "irc"), ("vlc", "mus")]

-- | Mapping of application name to workspace name where they should be shift to.
shiftWSApps :: M.HashMap String WorkspaceId
shiftWSApps = M.fromList [("de.haeckerfelix.Shortwave", "mus")]

-- | Set of class names which should be opened as centered float window.
centerFloatClasses :: S.HashSet String
centerFloatClasses = S.fromList
    [ "1Password"
    , "Blueman-adapters"
    , "Blueman-applet"
    , "Blueman-manager"
    , "fcitx5-config-qt"
    , "Gnome-logs"
    , "Gpick"
    , "Gwe"
    , "Gxmessage"
    , "Kvantum Manager"
    , "Nitrogen"
    , "Nm-connection-editor"
    , "Pamac-manager"
    , "Paradox Launcher"
    , "Pavucontrol"
    , "Pick-colour-picker"
    , "Qalculate-gtk"
    , "qt5ct"
    , "Solaar"
    , "Steam"
    , "Variety"
    , "Viewnior"
    , "Xmessage"
    ]

-- | Set of application names which should be opened as centered float window.
centerFloatApps :: S.HashSet String
centerFloatApps = S.fromList ["AlacrittyFloat", "font-manager"]
