{-# LANGUAGE LambdaCase #-}
----------------------------------------------------------------------------------------------------
-- |
-- Module      : Custom.Keymap.MajorKeymap
-- Copyright   : (c) Lucius Hu, 2020
-- License     : BSD3
-- Maintainer  : Lucius Hu <lebensterben@users.noreply.github.com>
--
-- Defines the major (global) keymap.
----------------------------------------------------------------------------------------------------

module Custom.Keymap.MajorKeymap (myMajorKeymap) where

import           Custom.Keymap                            ( mkNamedKeymap' )
import           Custom.Prompt                            ( searchWithInput
                                                          , searchWithSelection
                                                          , spawnPrompt
                                                          )
import           Custom.Variables                         ( myScratchPads
                                                          , myWorkspaces
                                                          )
import           Custom.Workspaces                        ( WSFilter(..)
                                                          , mergeToMaster
                                                          , moveToScreen
                                                          , moveToWS
                                                          , moveToWS'
                                                          , shiftToScreen
                                                          , shiftToWS
                                                          , shiftToWS'
                                                          , smartSink
                                                          , smartSinkAll
                                                          )
import           Data.List                                ( foldl' )
import           XMonad                                   ( ChangeLayout(NextLayout)
                                                          , IncMasterN(IncMasterN)
                                                          , KeyMask
                                                          , KeySym
                                                          , Resize(..)
                                                          , X
                                                          , XConf(config)
                                                          , XConfig(terminal)
                                                          , asks
                                                          , sendMessage
                                                          , spawn
                                                          , windows
                                                          , withFocused
                                                          )
import           XMonad.Actions.CopyWindow                ( kill1 )
import           XMonad.Actions.CycleWindows              ( rotUnfocusedDown )
import           XMonad.Actions.Navigation2D              ( windowGo
                                                          , windowSwap
                                                          )
import           XMonad.Actions.RotSlaves                 ( rotAllDown )
import           XMonad.Actions.SwapPromote               ( swapHybrid' )
import           XMonad.Actions.WithAll                   ( killAll )
import           XMonad.Hooks.ManageDocks                 ( ToggleStruts(..) )
import           XMonad.Layout.LimitWindows               ( decreaseLimit
                                                          , increaseLimit
                                                          )
import qualified XMonad.Layout.MultiToggle               as MT
import           XMonad.Layout.MultiToggle.Instances      ( StdTransformers(..) )
import           XMonad.Layout.ResizableTile              ( MirrorResize(..) )
import           XMonad.Layout.Spacing                    ( decScreenSpacing
                                                          , decWindowSpacing
                                                          , incScreenSpacing
                                                          , incWindowSpacing
                                                          )
import           XMonad.Layout.SubLayouts                 ( GroupMsg(..)
                                                          , onGroup
                                                          )
import qualified XMonad.Layout.ToggleLayouts             as T
import qualified XMonad.StackSet                         as W
import           XMonad.Util.NamedActions                 ( NamedAction
                                                          , (^++^)
                                                          )
import           XMonad.Util.NamedScratchpad              ( namedScratchpadAction )
import           XMonad.Util.Run                          ( runInTerm
                                                          , safeSpawn
                                                          , safeSpawnProg
                                                          )
import           XMonad.Util.Types                        ( Direction1D(..)
                                                          , Direction2D(..)
                                                          )
----------------------------------------------------------------------------------------------------
-- * Major Keymap
----------------------------------------------------------------------------------------------------

-- | A tree of key-sequence-to-named-actions pairs.
myMajorKeymap :: XConfig l -> [((KeyMask, KeySym), NamedAction)]
myMajorKeymap conf =
    foldl' (^++^) mempty
        . map (`mkNamedKeymap'` conf)
        $ [ xmonadManagement
          , windowNavigation
          , windowResize
          , workspaceNavigation
          , killWindow
          , layoutCmds
          , promptCommands
          , quickLaunch
          , emacsCmds
          , multiMediaKeys
          ]

-- | Manage XMonad
--
-- [@M-x M-r@]: Restart XMonad.
-- [@M-x M-q@]: Quit XMonad.
xmonadManagement :: [(String, String, X ())]
xmonadManagement =
    [ ("M-x " ++ suffix, desc, action)
    | (suffix, desc, action) <-
        [ ("M-r", "Restart XMonad", safeSpawn "xmonadctl" ["restart-wm"])
        , ("M-q", "Quit XMonad"   , safeSpawn "xmonadctl" ["quit-wm"])
        ]
    ]

-- | Window Navigation Keys
--
-- [@M-\<h, j, k, l\>@]: Move focus to the window in that direction.
-- [@M-S-\<h, j, k, l\>@]: Swap focused window with another window in that direction.
-- [@M-m@]: Move focus to master window
-- [@M-S-m@]: Swap focused window with master window
-- [@M-r@]: Rotate all windows except master
-- [@M-S-r@]: Rotate all windows
-- [@M-\<Tab\>@]: Window switcher. Type @\<Return>@ to switch to the window.
-- Or type @S-\<Return\>@ to bring the window to current workspace.
windowNavigation :: [(String, String, X ())]
windowNavigation =
    [ (prefix ++ suffix, desc ++ " " ++ descDir dir, action dir)
    | (prefix, desc, action) <-
        [ ("M-"  , "Move focus"         , flip windowGo False)
        , ("M-S-", "Swap focused window", flip windowSwap False)
        ]
    , (suffix, dir) <- [("h", L), ("j", D), ("k", U), ("l", R)]
    ]
    ++ [ ("M-<Tab>", "Window Switcher"                       , safeSpawnProg "rofi-window-switcher")
       , ("M-m"    , "Focus master window"                   , windows W.focusMaster)
       , ("M-S-m"  , "Swap master window with focused window", swapHybrid' False)
       , ("M-r"    , "Rotate other windows"                  , rotUnfocusedDown)
       , ("M-S-r"  , "Rotate all window"                     , rotAllDown)
       ]
  where
    descDir = \case
        L -> "Left"
        D -> "Down"
        U -> "Up"
        R -> "Right"

-- | Window Resizing Keys
--
-- [@M-M1-\<h, j, k, l>@]: Resizing by shrinking windows on the specified direction w.r.t
-- current focus.
windowResize :: [(String, String, X ())]
windowResize = map f [("h", L, Shrink), ("l", R, Expand)]
    ++ map f [("j", D, MirrorShrink), ("k", U, MirrorExpand)]
  where
    f (suffix, dir, action) =
        ("M-M1-" ++ suffix, "Shrink window on the " ++ show dir, sendMessage action)

-- NOTE: left here
-- | Workspace Navigation Keys
--
-- [@M-C-\<h, j, k, l>@]: Shift focus to the workspace in that direction.
-- [@M-S-C-\<j, k>@]: Move focused window to the workspace in that direction.
-- [@M-C-\<h, l>@]: Shift focus to the screen in that direction.
-- [@M-S-C-\<h, l>@]: Move focused window to the screen in that direction.
-- [@M-\<1..9>@]: Jump to i-th workspace.
-- [@M-S-\<1..9>@]: Bring focused window to i-th workspace.
workspaceNavigation :: [(String, String, X ())]
workspaceNavigation =
    [ (prefix ++ suffix, descPre ++ " " ++ desc ++ " " ++ descSuf, action)
    | (prefix, descPre, descSuf, source) <-
        [ ("M-C-"  , "Go to"                  , "Workspace", moveToWS d1WSmapping NonScratchPad)
        , ("M-S-C-", "Shift focused window to", "Workspace", shiftToWS d1WSmapping NonScratchPad)
        , ("M-C-"  , "Go to"                  , "Screen"   , moveToScreen d1SCmapping)
        , ("M-S-C-", "Shift focused window to", "Screen"   , shiftToScreen d1SCmapping)
        , ("M-"    , "Go to"                  , "Workspace", moveToWS' myWorkspaces)
        , ("M-S-"  , "Shift focused window to", "Workspace", shiftToWS' myWorkspaces)
        ]
    , (suffix, desc, action)             <- source
    ]
  where
    d1WSmapping = [("j", Next), ("k", Prev)]
    d1SCmapping = [("h", Prev), ("l", Next)]

-- | Kill Windows
--
-- [@M-q@]: Kill focused window.
-- [@M-S-q@]: Kill all windows on current workspace.
killWindow :: [(String, String, X ())]
killWindow =
    [("M-q", "Kill focused window", kill1), ("M-S-q", "Kill all windows on current WS", killAll)]

-- | Layout Commands
--
-- [@M-`@]: Switch to next layout.
-- [@M-S-`@]: Toggle float layout.
-- [@M-C-m@]: Merge focused window with master window.
-- [@M-C-S-m@]: Merge all windows.
-- [@M-\[@]: Previous tab.
-- [@M-\]@]: Next tab
-- [@M-s@]: Sink focused window if it's floating, otherwise unmerge it if it's @Tabbed@.
-- [@M-S-s]: Unsink all floating windows in current workspace if there's any, otherwise
-- unmerge all windows in @Tabbed@ layout in current workspace.
-- [@M-\<F11\>@]: Toggle fullscreen layout.
-- [@M-S-\<F11\>@]: Toggle status bar.
-- [@M-\<KP_Add\/KP_Subtract\>@]: Increase\/Decrease number of master windows.
-- [@M-S-\<KP_Add\/KP_Subtract\>@]: Increase\/Decrease number of visible windows.
-- [@M-\<-, =\>@]: Decrease\/Increase window gap.
-- [@M-S-\<-, =\>@]: Decrease\/Increase screen gap.
layoutCmds :: [(String, String, X ())]
layoutCmds =
    [ ("M-`"    , "Cycle layouts"            , sendMessage NextLayout)
    , ("M-S-`"  , "Toggles 'floats' layout"  , sendMessage (T.Toggle "floats"))
    , ("M-C-m"  , "Merge focused with master", mergeToMaster)
    , ("M-C-S-m", "Merge all"                , withFocused (sendMessage . MergeAll))
    , ("M-["    , "Previous tab"             , onGroup W.focusUp')
    , ("M-]"    , "Next tab"                 , onGroup W.focusDown')
    , ("M-s"    , "Sink focused if floating, otherwise unmerge it", smartSink)
    , ("M-S-s", "Sink all floating windows if any, otherwise unmerge all", smartSinkAll)
    , ("M-<F11>", "Toggles fullscreen layout", sendMessage (MT.Toggle NBFULL))
    , ( "M-S-<F11>"
      , "Toggles status bar"
      , safeSpawn "polybar-msg" ["cmd", "toggle"] >> sendMessage ToggleStruts
      )
    , ("M-<KP_Add>"       , "More master windows"  , sendMessage (IncMasterN 1))
    , ("M-<KP_Subtract>"  , "Fewer master windows" , sendMessage (IncMasterN (-1)))
    , ("M-S-<KP_Add>"     , "More visible windows" , increaseLimit)
    , ("M-S-<KP_Subtract>", "Fewer visible windows", decreaseLimit)
    , ("M--"              , "Increase window gap"  , decWindowSpacing 4)
    , ("M-="              , "Decrease window gap"  , incWindowSpacing 4)
    , ("M-S--"            , "Increase screen gap"  , decScreenSpacing 4)
    , ("M-S-="            , "Decrease screen gap"  , incScreenSpacing 4)
    ]

-- | Spawning Prompts
--
--
-- === Search prompt.
-- [@M-/@]: Search prompt, followed by one of the following key.
--
--     [@a@]: Arch Linux Wiki.
--     [@S-a@]: Arch Linux Package.
--     [@c@]: Clear Linux Package.
--     [@S-c@]: Clear Linux Bundle.
--     [@d@]: DuckDuckGo.
--     [@g@]: Google.
--     [@h@]: Hoogle.
--     [@i@]: Google Image.
--     [@m@]: Google Maps.
--     [@t@]: Thesaurus.
--     [@w@]: Wikipedia.
--     [@S-w@]: Wiktionary.
--     [@y@]: Youtube.
--     [@z@]: Wikipedia (zh).
--     [@S-z@]: Wiktionary (zh).
--     [@\$@]: Amazon.
--     [@\/@]: Choose a search engine..
--
-- [@M-S-/@]: Same as search prompt, but with current selection as query.
--
-- === Other Prompt
-- [@M-\<Space>@]: Other prompt.
--
--     [@m@]: Manpage.
--     [@p@]: Password store.
--     [@=@]: Qalc.
promptCommands :: [(String, String, X ())]
promptCommands =
    [ (prefix ++ " " ++ suffix, desc, action)
    | (prefix, source)       <-
        [("M-<Space>", spawnPrompt), ("M-/", searchWithInput), ("M-S-/", searchWithSelection)]
    , (suffix, desc, action) <- source
    ]

-- TODO: Still expanding
-- | Quick Launch
--
-- [@M-\<Return\>@]: Open terminal.
-- [@M-S-\<Return\>@]: Open terminal pop-up.
-- [@M-o@]: App Launcher.
-- [@M-b@]: Open browser.
-- [@M-f@]: Open file manager.
-- [@M-S-\<Esc\>@]: Open Htop.
-- [@M-\<F13\>@]: Screenshot whole screen.
-- [@M-S-\<F13\>@]: Screenshot selected region or window.
quickLaunch :: [(String, String, X ())]
quickLaunch =
    [ ("M-<Return>"  , "Open terminal"          , asks (terminal . config) >>= safeSpawnProg)
        , ("M-S-<Return>", "Open terminal in pop-up", namedScratchpadAction myScratchPads "terminal")
        , ("M-o"         , "App Launcher"           , safeSpawnProg "rofi-launcher")
        , ("M-b"         , "Open browser"           , safeSpawnProg "firefox")
        , ("M-f"         , "Open file manager"      , safeSpawnProg "nautilus")
        , ( "M-S-<Esc>"
          , "Open htop"
          , runInTerm
              "--class 'Alacritty Float','Alacritty Float' \
                   \-o window.dimensions.columns=200 \
                   \-o window.dimensions.lines=40"
              "htop"
          )
        ]
        ++ [ (key, desc, spawn $ "polybar-scrot -v viewnior " ++ flag)
           | (key, desc, flag) <-
               [ ("M-<F13>"  , "Screenshot whole screen"             , "")
               , ("M-S-<F13>", "Screenshot selected region or window", "-s")
               ]
           ]

-- TODO: Still expanding
-- | Emacs Commands
--
-- [@M-e e@]: Open Emacs.
-- [@M-e b@]: List Emacs buffers.
-- [@M-e d@]: Dired.
-- [@M-e i@]: erc.
-- [@M-e m@]: mu4e.
-- [@M-e n@]: Elfeed.
emacsCmds :: [(String, String, X ())]
emacsCmds =
    [ ("M-e " ++ suffix, desc, safeSpawn "emacsclient" args')
    | (suffix, desc, args) <-
        [ ("e", "Open Emacs"        , [])
        , ("b", "List Emacs buffers", ["(ibuffer)"])
        , ("d", "Dired"             , ["(dired nil)"])
        , ("i", "Emacs IRC"         , ["(erc)"])
        , ("m", "mu4e"              , ["(mu4e)"])
        , ("n", "Elfeed RSS"        , ["(elfeed)"])
        ]
    , let args' = ["-c", "-a=emacs"] ++ case args of
              [] -> []
              _  -> "--eval" : args
    ]

-- TODO: Add Play Next Prev keys
-- | Multi Media Keys
--
-- [@\<XF86AudioLowerVolume\>@]: Lower volume by 5%.
-- [@\<XF86AudioRaiseVolume\>@]: Raise volume by 5%.
-- [@\<XF86AudioMute\>@]: Toggle volume.
multiMediaKeys :: [(String, String, X ())]
multiMediaKeys =
    [ (key, desc, spawn $ "polybar-msg action '#pulseaudio." ++ message ++ "'")
    | (key, desc, message) <-
        [ ("<XF86AudioLowerVolume>", "Lower volume by 5%", "dec")
        , ("<XF86AudioRaiseVolume>", "Raise volume by 5%", "inc")
        , ("<XF86AudioMute>"       , "Toggle volume"     , "toggle")
        ]
    ]
    ++ [ (key, desc, safeSpawn "mpc" [action])
       | (key, desc, action) <-
           [ ("<XF86AudioPrev>", "Toggle play/pause", "prev")
           , ("<XF86AudioPlay>", "Toggle play/pause", "toggle")
           , ("<XF86AudioNext>", "Toggle play/pause", "next")
           ]
       ]

    -- FIXME: I don't use mocp
        -- , ( "M-C-c"
        --   , namedScratchpadAction myScratchPads "mocp"
        --   )
    -- Controls for mocp music player (SUPER-u followed by a key)
        -- , ("M-u p", spawn "mocp --play")
        -- , ("M-u l", spawn "mocp --next")
        -- , ("M-u h", spawn "mocp --previous")
        -- , ( "M-u <Space>"
        --   , spawn "mocp --toggle-pause"
        --   )

    -- FIXME
    --- My Applications (Super+Alt+Key)
        -- , ("M-M1-a", spawn (myTerminal ++ ["ncpamixer"]))
        -- , ("M-M1-b", spawn "surf www.youtube.com/c/DistroTube/")
        -- , ("M-M1-e", spawn (myTerminal ++ "neomutt"))
        -- , ("M-M1-f", spawn (myTerminal ++ "sh ./.config/vifm/scripts/vifmrun | bash"))
        -- , ("M-M1-i", spawn (myTerminal ++ "irssi"))
        -- , ("M-M1-j", spawn (myTerminal ++ "joplin"))
        -- , ("M-M1-l", spawn (myTerminal ++ "lynx https://distrotube.com"))
        -- , ("M-M1-m", spawn (myTerminal ++ "mocp"))
        -- , ("M-M1-n", spawn (myTerminal ++ "newsboat"))
        -- , ("M-M1-p", spawn (myTerminal ++ "pianobar"))
        -- , ("M-M1-r", spawn (myTerminal ++ "rtv"))
        -- , ("M-M1-t", spawn (myTerminal ++ ["toot curses"]))
        -- , ("M-M1-w", spawn (myTerminal ++ ["wopr report.xml"]))
        -- , ( "M-M1-y"
          -- , spawn (myTerminal ++ " -e youtube-viewer")
          -- )

    -- Multimedia Keys
        -- FIXME Bug prevents it from toggling correctly in 12.04.
        -- , ("<XF86HomePage>"        , spawn "firefox")
        -- , ("<XF86Search>", safeSpawn "firefox" ["https://www.google.com/"])
        -- , ("<XF86Mail>", runOrRaise "geary" (resource =? "thunderbird"))
        -- , ("<XF86Calculator>", runOrRaise "gcalctool" (resource =? "gcalctool"))
        -- , ("<XF86Eject>"           , spawn "toggleeject")
        -- , ("<Print>"               , spawn "scrotd 0")
