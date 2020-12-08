----------------------------------------------------------------------------------------------------
-- |
-- Module      : Custom.Keymap.MajorKeymap
-- Copyright   : (c) Lucius Hu, 2020
-- License     : BSD3
-- Maintainer  : Lucius Hu <lebensterben@users.noreply.github.com>
--
-- Defines the global keymap.
----------------------------------------------------------------------------------------------------

module Custom.Keymap.MajorKeymap (myMajorKeymap) where

import           Custom.GridSelect                        ( gridWS
                                                          , gridGoTo
                                                          , gridBring
                                                          , gridSpawn
                                                          )
import           Custom.Keymap                            ( mkNamedKeymapSection )
import           Custom.Prompt                            ( confirmExit
                                                          , shellPrompt
                                                          , spawnPrompt
                                                          , searchWithInput
                                                          , searchWithSelection
                                                          )
import           Custom.TreeSelect                        ( treeSelect )
import           Custom.Util.Apps                         ( DefaultApps(..) )
import           Custom.Variables                         ( myWorkspaces
                                                          , openWith
                                                          )
import           Custom.Windows                           ( focusWindow
                                                          , shrinkWindowTo
                                                          , swapWindow
                                                          )
import           Custom.Workspaces                        ( moveToWS
                                                          , moveToWS'
                                                          , shiftToWS
                                                          , shiftToWS'
                                                          , moveToScreen
                                                          , shiftToScreen
                                                          , WSFilter(..)
                                                          )
import           Data.List                                ( foldl' )
import           XMonad                                   ( KeySym
                                                          , KeyMask
                                                          , sendMessage
                                                          , windows
                                                          , withFocused
                                                          , X
                                                          , XConfig
                                                          , ChangeLayout(NextLayout)
                                                          , IncMasterN(IncMasterN)
                                                          )
import           XMonad.Actions.CopyWindow                ( kill1 )
import           XMonad.Actions.CycleWindows              ( rotUnfocusedDown )
import           XMonad.Actions.RotSlaves                 ( rotAllDown )
import           XMonad.Actions.SwapPromote               ( swapHybrid' )
import           XMonad.Actions.WithAll                   ( sinkAll
                                                          , killAll
                                                          )
import           XMonad.Hooks.ManageDocks                 ( ToggleStruts(..) )
import           XMonad.Layout.LimitWindows               ( increaseLimit
                                                          , decreaseLimit
                                                          )
import qualified XMonad.Layout.MultiToggle               as MT
import           XMonad.Layout.MultiToggle.Instances      ( StdTransformers(..) )
import           XMonad.Layout.Spacing                    ( incScreenSpacing
                                                          , decScreenSpacing
                                                          , incWindowSpacing
                                                          , decWindowSpacing
                                                          )
import           XMonad.Layout.SubLayouts                 ( onGroup
                                                          , GroupMsg(..)
                                                          )
import qualified XMonad.Layout.ToggleLayouts             as T
import qualified XMonad.StackSet                         as W
import           XMonad.Util.NamedActions                 ( (^++^)
                                                          , NamedAction
                                                          )
import           XMonad.Util.Run                          ( safeSpawn )
import           XMonad.Util.Types                        ( Direction1D(..)
                                                          , Direction2D(..)
                                                          )

----------------------------------------------------------------------------------------------------

-- | Manage XMonad
--
-- [@M-x S-r@]: Restart XMonad.
-- [@M-x S-H-q@]: Quit XMonad.
xmonadManagement :: XConfig l -> (String, [(String, String, X ())])
xmonadManagement _ =
    ( "XMonad"
    , [ ("M-x " ++ k, dscr, v)
      | (k, dscr, v) <-
          [ ( "M-r"
            , "Restart XMonad"
            , safeSpawn "xmonad" ["--recompile"] >> safeSpawn "xmonad" ["--restart"]
            )
          , ("M-M4-q", "Quit XMonad", confirmExit)
          ]
      ]
    )

-- | Window Navigation Keys
--
-- [@M-\<h, j, k, l>@]: Move focus to the window in that direction.
-- [@M-S-\<h, j, k, l>@]: Swap focused window with another window in that direction.
-- [@M-m@]: Move focus to master window
-- [@M-S-m@]: Swap focused window with master window
-- [@M-r@]: Rotate all windows except master
-- [@M-S-r@]: Rotate all windows
windowNavigation :: XConfig l -> (String, [(String, String, X ())])
windowNavigation _ =
    ( "Window Navigation"
    , concat
        [ map (\(k, dir, x) -> (k, "Move focus to " ++ show dir, x)) $ focusWindow "M-" d2mapping
        , map (\(k, dir, x) -> (k, "Swap focus to " ++ show dir, x)) $ swapWindow "M-S-" d2mapping
        , [ ("M-m"  , "Focus master window" , windows W.focusMaster)
          , ("M-S-m", "Swap master window"  , swapHybrid' False)
          , ("M-r"  , "Rotate other windows", rotUnfocusedDown)
          , ("M-S-r", "Rotate all window"   , rotAllDown)
          ]
        ]
    )
    where d2mapping = [("h", L), ("j", D), ("k", U), ("l", R)]

-- | Workspace Navigation Keys
--
-- [@M-C-\<j, k>@]: Shift focus to the workspace in that direction.
-- [@M-S-C-\<j, k>@]: Move focused window to the workspace in that direction.
-- [@M-C-\<h, l>@]: Shift focus to the screen in that direction.
-- [@M-S-C-\<h, l>@]: Move focused window to the screen in that direction.
-- [@M-<1..9>@]: Jump to i-th workspace.
-- [@M-S-<1..9>@]: Bring focused window to i-th workspace.
workspaceNavigation :: XConfig l -> (String, [(String, String, X ())])
workspaceNavigation _ =
    ( "Workspace Navigation"
    , concat
        [ map (\(k, dir, x) -> (k, "Go to " ++ show dir ++ "WS", x))
            $ moveToWS "M-C-" d1wsmapping NonScratchPad
        , map (\(k, dir, x) -> (k, "Shift focused to " ++ show dir ++ "WS", x))
            $ shiftToWS "M-S-C-" d1wsmapping NonScratchPad
        , map (\(k, dir, x) -> (k, "Go to " ++ show dir ++ "Screnn", x))
            $ moveToScreen "M-C-" d1scmapping
        , map (\(k, dir, x) -> (k, "Shift focused to " ++ show dir ++ "Screen", x))
            $ shiftToScreen "M-S-C-" d1scmapping
        , map (\(k, i, x) -> (k, "Go to " ++ show i ++ "-th WS", x)) $ moveToWS' myWorkspaces "M-"
        , map (\(k, i, x) -> (k, "Shift focused to " ++ show i ++ "-th WS", x))
            $ shiftToWS' myWorkspaces "M-S-"
        ]
    )
  where
    d1wsmapping = [("j", Next), ("k", Prev)]
    d1scmapping = [("h", Prev), ("l", Next)]

-- | Window Resizing Keys
--
-- [@M-M1-\<h, j, k, l>@]: Resizing by shrinking windows on the specified direction w.r.t
-- current focus.
windowResize :: XConfig l -> (String, [(String, String, X ())])
windowResize _ =
    ( "Window Resize"
    , map (\(k, dir, x) -> (k, "Shrink window on the " ++ show dir, x))
        $ shrinkWindowTo "M-M1-" [("h", L), ("j", D), ("k", U), ("l", R)]
    )

-- | Border Spacing
--
-- [@M-\<-, =>@]: Decrease\/Increase window gap.
-- [@M-S-\<-, =>@]: Decrease\/Increase screen gap.
borderSpacing :: XConfig l -> (String, [(String, String, X ())])
borderSpacing _ =
    ( "Border Spacing"
    , [ ("M--"  , "Increase window gap", decWindowSpacing 4)
      , ("M-="  , "Decrease window gap", incWindowSpacing 4)
      , ("M-S--", "Increase screen gap", decScreenSpacing 4)
      , ("M-S-=", "Decrease screen gap", incScreenSpacing 4)
      ]
    )

-- | Kill Windows
--
-- [@M-q@]: Kill focused window.
-- [@M-S-q@]: Kill all windows on current workspace.
killWindow :: XConfig l -> (String, [(String, String, X ())])
killWindow _ =
    ( "Kill Windows"
    , [("M-q", "Kill focused window", kill1), ("M-S-q", "Kill all windows on current WS", killAll)]
    )

-- | Layout Commands
--
-- [@M-\<Tab>@]: Switch to next layout.
-- [@M-S-\<Tab>@]: Reset current layout.
-- [@M-\<F11>@]: Toggle fullscreen with struts.
-- [@M-S-\<F11>@]: Toggle struts without struts.
-- [@M-\<KP_Add\/KP_Subtract>@]: Increase\/Decrease number of master windows.
-- [@M-S-\<KP_Add\/KP_Subtract>@]: Increase\/Decrease number of visible windows.
layoutCmds :: XConfig l -> (String, [(String, String, X ())])
layoutCmds _ =
    ( "Layouts"
    , [ ("M-`"    , "Cycle layouts"                 , sendMessage NextLayout)
      , ("M-S-`"  , "Toggles 'floats' layout"       , sendMessage (T.Toggle "floats"))
      , ("M-s"    , "Sink focued window"            , withFocused $ windows . W.sink)
      , ("M-S-s"  , "Sink all windows in current WS", sinkAll)
      , ("M-<F11>", "Toggles fullscreen w/ struts"  , sendMessage (MT.Toggle NBFULL))
      , ( "M-S-<F11>"
        , "Toggles fullscreen w/o struts"
        , sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts
        )
      , ("M-<KP_Add>"       , "More master windows"  , sendMessage (IncMasterN 1))
      , ("M-<KP_Subtract>"  , "Fewer master windows" , sendMessage (IncMasterN (-1)))
      , ("M-S-<KP_Add>"     , "More visible windows" , increaseLimit)
      , ("M-S-<KP_Subtract>", "Fewer visible windows", decreaseLimit)
      ]
    )

-- | Sublayout Commands
--
-- [@M-C-m@]: Merge all windows.
-- [@M-C-u@]: Unmerge focused window.
-- [@M-C-S-u@]: Unmerge all windows.
-- [@M-\[@]: Previous tab.
-- [@M-\]@]: Next tab
subLayoutCmds :: XConfig l -> (String, [(String, String, X ())])
subLayoutCmds _ =
    ( "Sub Layouts"
    , [ ("M-C-m"  , "Merge all"      , withFocused (sendMessage . MergeAll))
      , ("M-C-u"  , "Unmerge focused", withFocused (sendMessage . UnMerge))
      , ("M-C-S-u", "Unmerge all"    , withFocused (sendMessage . UnMergeAll))
      , ("M-["    , "Previous tab"   , onGroup W.focusUp')
      , ("M-]"    , "Next tab"       , onGroup W.focusDown')
      ]
    )

-- | Grid Select
--
-- [@M-o@]: Grid select launcher.
-- [@M-g@]: Grid select go to window.
-- [@M-S-g@]: Grid select bring window.
-- [@M-C-g@]: Grid select go to workspace
gridSelectMenu :: XConfig l -> (String, [(String, String, X ())])
gridSelectMenu _ =
    ( "Grid Select"
    , [ ("M-o"  , "Grid Select: Apps"             , gridSpawn)
      , ("M-g"  , "Grid Select: Go to window"     , gridBring)
      , ("M-S-g", "Grid Select: Bring window"     , gridGoTo)
      , ("M-C-g", "Grid Selected: Go to workspace", gridWS)
      ]
    )

-- | Tree Select
--
-- [@M-t@]: Tree select.
treeSelectMenu :: XConfig l -> (String, [(String, String, X ())])
treeSelectMenu c = ("Tree Select", [("M-t", "Tree Select", treeSelect c)])

-- | Spawning Prompts
--
-- [@M-S-\<Return>@]: Shell prompt.
-- [@M-\<Space>@]: Other prompt.
--
--     [@m@]: A prompt for manpages.
--     [@s@]: A ssh prompt.
--     [@=@]: A calculator prompt.
--
-- [@M-/@]: Search prompt.
-- [@M-S-/@]: Search prompt with current selection.
--
--     [@a@]: Arch Wiki.
--     [@c@]: Clearlinux-pkgs.
--     [@d@]: Vocabulary.com.
--     [@g@]: Google.
--     [@h@]: Hoogle.
--     [@i@]: Google Image.
--     [@m@]: Google Maps.
--     [@t@]: Thesaurus.
--     [@y@]: Youtube.
--     [@$@]: Amazon.
promptCommands :: XConfig l -> (String, [(String, String, X ())])
promptCommands _ =
    ( "Prompts"
    , concat
        [ [("M-S-<Return>", "Shell Prompt", shellPrompt)]
        , [ ("M-<Space> " ++ k, dscr, f) | (k, dscr, f) <- spawnPrompt ]
        , [ ("M-/ " ++ k, "Search on " ++ name, f) | (k, name, f) <- searchWithInput ]
        , [ ("M-S-/ " ++ k, "Search w/ selection on " ++ name, f)
          | (k, name, f) <- searchWithSelection
          ]
        ]
    )

-- TODO: Still expanding
-- | Quick Launch
--
-- [@M-\<Return>@]: Open terminal.
-- [@M-b@]: Open browser.
-- [@M-f@]: Open file manager.
-- [@M-S-\<Esc>@]: Open Htop.
quickLaunch :: XConfig l -> (String, [(String, String, X ())])
quickLaunch _ =
    ( "Quick Launch"
    , [ ("M-<Return>", "Open terminal"    , openWith myTerminal [])
      , ("M-b"       , "Open browser"     , openWith myBrowser [])
      , ("M-f"       , "Open file manager", openWith myFileManager [])
      , ("M-S-<Esc>" , "Open htop"        , openWith myTerminal ["htop"])
      ]
    )

-- | Emacs Commands
--
-- [@M-e e@]: Open Emacs.
-- [@M-e b@]: List Emacs buffers.
-- [@M-e d@]: Dired.
-- [@M-e i@]: erc.
-- [@M-e m@]: mu4e.
-- [@M-e n@]: Elfeed.
emacsCmds :: XConfig l -> (String, [(String, String, X ())])
emacsCmds _ =
    ( "Emacs"
    , [ ("M-e e", "Open Emacs"        , openWith myEditor [])
      , ("M-e b", "List Emacs buffers", openWith myEditor ["'(ibuffer)'"])
      , ("M-e d", "Dired"             , openWith myEditor ["'(dired nil)'"])
      , ("M-e i", "Emacs IRC"         , openWith myEditor ["'(erc)'"])
      , ("M-e m", "mu4e"              , openWith myEditor ["'(mu4e)'"])
      , ("M-e n", "Elfeed RSS"        , openWith myEditor ["'(elfeed)'"])
      ]
    )

----------------------------------------------------------------------------------------------------
-- Major Keymap
----------------------------------------------------------------------------------------------------

-- | A tree of key-sequence-to-named-actions pairs.
--
-- Note: The order matters here, as it determines the output of 'Custom.Keymap.showKeyBindings'.
myMajorKeymap :: XConfig l -> [((KeyMask, KeySym), NamedAction)]
myMajorKeymap conf = foldl'
    (\x xs -> x ^++^ uncurry mkNamedKeymapSection (xs conf) conf)
    []
    [ xmonadManagement
    , windowNavigation
    , workspaceNavigation
    , windowResize
    , borderSpacing
    , killWindow
    , layoutCmds
    , subLayoutCmds
    , gridSelectMenu
    , treeSelectMenu
    , promptCommands
    , quickLaunch
    , emacsCmds
    ]

    -- FIXME: I don't use mocp
    -- Scratchpads
        -- , ("M-C-<Return>", namedScratchpadAction myScratchPads "terminal")
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
        -- , ("<XF86AudioPlay>", openWith myTerminal ["mocp", "--play"])
        -- , ("<XF86AudioPrev>", openWith myTerminal ["mocp", "--previous"])
        -- , ( "<XF86AudioNext>"
        --   , openWith myTerminal ["mocp", "--next"]
        --   )
        -- FIXME Bug prevents it from toggling correctly in 12.04.
        -- , ("<XF86AudioMute>",   spawn "amixer set Master toggle")
      -- , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
      -- , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
        -- , ("<XF86HomePage>"        , spawn "firefox")
        -- , ("<XF86Search>", safeSpawn "firefox" ["https://www.google.com/"])
        -- , ("<XF86Mail>", runOrRaise "geary" (resource =? "thunderbird"))
        -- , ("<XF86Calculator>", runOrRaise "gcalctool" (resource =? "gcalctool"))
        -- , ("<XF86Eject>"           , spawn "toggleeject")
        -- , ("<Print>"               , spawn "scrotd 0")
        -- \^++^ xmonadManagement
        -- \^++^ windowNavigation
        -- \^++^ workspaceNavigation
        -- \^++^ windowResize
        -- \^++^ windowSpacing
        -- \^++^ layoutCommands
        -- \^++^ promptCommands
