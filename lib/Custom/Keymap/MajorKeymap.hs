----------------------------------------------------------------------------------------------------
-- |
-- Module      : Custom.Keymap.MajorKeymap
-- Copyright   : (c) Lucius Hu, 2020
-- License     : BSD3
-- Maintainer  : Lucius Hu <lebensterben@users.noreply.github.com>
--
-- Defines the keymap for the main interface.
----------------------------------------------------------------------------------------------------

module Custom.Keymap.MajorKeymap (majorKeymap) where

import           Custom.GridSelect                        ( gridWS
                                                          , gridGoTo
                                                          , gridBring
                                                          , gridSpawn
                                                          )
import           Custom.Prompt                            ( confirmExit
                                                          , shellPrompt
                                                          , spawnPrompt
                                                          , searchWithInput
                                                          , searchWithSelection
                                                          )
import           Custom.TreeSelect                        ( treeSelect )
import           Custom.Variables                         ( myWorkspaces
                                                          , myBrowser
                                                          , myTerminal
                                                          , myEditor
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
import           XMonad                                   ( spawn
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
                                                          , pushGroup
                                                          , GroupMsg(..)
                                                          )
import qualified XMonad.Layout.ToggleLayouts             as T
import qualified XMonad.StackSet                         as W
import           XMonad.Util.Types                        ( Direction1D(..)
                                                          , Direction2D(..)
                                                          )

-- | Manage XMonad
--
-- [@M-x S-r@]: Restart XMonad.
-- [@M-x S-H-q@]: Quit XMonad.
xmonadManagement :: [(String, X ())]
xmonadManagement =
    [ ("M-x " ++ k, v)
    | (k, v) <-
        [ ("M-r", spawn "xmonad --recompile && xmonad --restart")
        , ("M-M4-q", confirmExit)
-- ++ [("M-x M-h", spawn ("echo " ++ show keyHelp ++ " | xmessage -file -"))] -- TODO
        ]
    ]

-- | Window Navigation Keys
--
-- [@M-\<h, j, k, l>@]: Move focus to the window in that direction.
-- [@M-S-\<h, j, k, l>@]: Swap focused window with another window in that direction.
-- [@M-m@]: Move focus to master window
-- [@M-S-m@]: Swap focused window with master window
-- [@M-r@]: Rotate all windows except master
-- [@M-S-r@]: Rotate all windows
windowNavigation :: [(String, X ())]
windowNavigation = concat
    [ focusWindow "M-" [("h", L), ("j", D), ("k", U), ("l", R)]
    , swapWindow "M-S-" [("h", L), ("j", D), ("k", U), ("l", R)]
    , [ ("M-m"  , windows W.focusMaster)
      , ("M-S-m", swapHybrid' False)
      , ("M-r"  , rotUnfocusedDown)
      , ("M-S-r", rotAllDown)
      ]
    ]

-- | Workspace Navigation Keys
--
-- [@M-C-\<j, k>@]: Shift focus to the workspace in that direction.
-- [@M-S-C-\<j, k>@]: Move focused window to the workspace in that direction.
-- [@M-C-\<h, l>@]: Shift focus to the screen in that direction.
-- [@M-S-C-\<h, l>@]: Move focused window to the screen in that direction.
-- [@M-<1..9>@]: Jump to i-th workspace.
-- [@M-S-<1..9>@]: Bring focused window to i-th workspace.
workspaceNavigation :: [(String, X ())]
workspaceNavigation = concat
    [ moveToWS "M-C-" [("j", Next), ("k", Prev)] NonScratchPad
    , shiftToWS "M-S-C-" [("j", Next), ("k", Prev)] NonScratchPad
    , moveToScreen "M-C-" [("h", Prev), ("l", Next)]
    , shiftToScreen "M-S-C-" [("h", Prev), ("l", Next)]
    , moveToWS' myWorkspaces "M-"
    , shiftToWS' myWorkspaces "M-S-"
    ]

-- | Window Resizing Keys
--
-- [@M-M1-\<h, j, k, l>@]: Resizing by shrinking windows on the specified direction w.r.t
-- current focus.
windowResize :: [(String, X ())]
windowResize = shrinkWindowTo "M-M1-" [("h", L), ("j", D), ("k", U), ("l", R)]

-- | Window Spacing
--
-- [@M-\<-, =>@]: Decrease\/Increase Window Gap
-- [@M-S-\<-, =>@]: Decrease\/Increase Screen Gap
windowSpacing :: [(String, X ())]
windowSpacing =
    [ ("M--"  , decWindowSpacing 4)
    , ("M-="  , incWindowSpacing 4)
    , ("M-S--", decScreenSpacing 4)
    , ("M-S-=", incScreenSpacing 4)
    ]

-- | Layout control
--
-- [@M-\<Tab>@]: Switch to next layout.
-- [@M-S-\<Tab>@]: Reset current layout.
-- [@M-\<F11>@]: Toggle fullscreen.
-- [@M-S-\<F11>@]: Toggle struts.
-- [@M-\<KP_Add\/KP_Subtract>@]: Increase\/Decrease number of master windows.
-- [@M-S-\<KP_Add\/KP_Subtract>@]: Increase\/Decrease number of visible windows.
layoutCommands :: [(String, X ())]
layoutCommands =
    [ ( "M-`"               -- Switch to next layout
      , sendMessage NextLayout
      )
    , ( "M-S-`"             -- Toggles 'floats' layout
      , sendMessage (T.Toggle "floats")
      )
    , ( "M-s"               -- Un-float focued window
      , withFocused $ windows . W.sink
      )
    , ( "M-S-s"             -- Un-float all windows in current ws
      , sinkAll
      )
    , ( "M-<F11>"           -- Toggles fullscreen
      , sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts
      )
    , ( "M-S-<F11>"         -- Toggles struts
      , sendMessage ToggleStruts
      )
    , ( "M-<KP_Add>"        -- Increase number of clients in master pane
      , sendMessage (IncMasterN 1)
      )
    , ( "M-<KP_Subtract>"   -- Decrease number of clients in master pane
      , sendMessage (IncMasterN (-1))
      )
    , ( "M-S-<KP_Add>"      -- Increase number of windows
      , increaseLimit
      )
    , ( "M-S-<KP_Subtract>" -- Decrease number of windows
      , decreaseLimit
      )
    ]

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
promptCommands :: [(String, X ())]
promptCommands = concat
    [ [("M-S-<Return>", shellPrompt)] -- shell prompt
    , [ ("M-<Space> " ++ k, f)        -- other xmonad prompts
      | (k, f) <- spawnPrompt
      ]
    , [ ("M-/ " ++ k, f)              -- search engines
      | (k, f) <- searchWithInput
      ]
    , [ ("M-S-/ " ++ k, f)            -- search engines w/ current selection
      | (k, f) <- searchWithSelection
      ]
    ]


-- TODO
-- | A tree of key-sequence-to-actions pairs.
majorKeymap :: XConfig l -> [(String, X ())]
majorKeymap conf = concat
    [ [
        -- Quick Launch
        ( "M-<Return>"                -- Launch Shell
        , openWith myTerminal []
        )
      , ( "M-b"                       -- Firefox
        , openWith myBrowser []
        )
      , ( "M-S-<Esc>"                 -- Htop
        , openWith myTerminal ["htop"]
        )

        -- Kill windows
      , ( "M-q"                       -- Kill the currently focused window
        , kill1
        )
      , ( "M-S-q"                     -- Kill all windows on current workspace
        , killAll
        )

      -- Grid Select
      , ( "M-g g"                     -- Grid select favorite apps
        , gridSpawn
        )
      , ( "M-g t"                     -- Go to selected window and its ws
        , gridBring
        )
      , ( "M-g b"                     -- Bring selected window to current ws
        , gridGoTo
        )
      , ( "M-<Tab>"                   -- Go to selected workspace
        , gridWS
        )

      -- Tree Select
      , ( "M-t"                       -- Open tree select menu
        , treeSelect conf
        )

    -- Sublayouts
    -- This is used to push windows to tabbed sublayouts, or pull them out of it.
           -- FIXME: The following are not working
      , ("M-<L>"  , sendMessage $ pushGroup L)
      , ("M-<R>"  , sendMessage $ pushGroup R)
      , ("M-<U>"  , sendMessage $ pushGroup U)
      , ("M-<D>"  , sendMessage $ pushGroup D)
      , ("M-C-m"  , withFocused (sendMessage . MergeAll))
      , ("M-C-u"  , withFocused (sendMessage . UnMerge))
      , ("M-C-S-u", withFocused (sendMessage . UnMergeAll))
      , ( "M-["
        , onGroup W.focusUp'
        )    -- Switch focus to prev tab
      , ( "M-]"
        , onGroup W.focusDown'
        )  -- Switch focus to next tab

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

    -- Emacs (Super-e followed by a key)
      , ( "M-e e"
        , openWith myEditor []
        )                            -- start emacs
      , ( "M-e b"
        , openWith myEditor ["'(ibuffer)'"]
        )         -- list emacs buffers
      , ( "M-e d"
        , openWith myEditor ["'(dired nil)'"]
        )       -- dired emacs file manager
      , ( "M-e i"
        , openWith myEditor ["'(erc)'"]
        )             -- erc emacs irc client
      , ( "M-e m"
        , openWith myEditor ["'(mu4e)'"]
        )            -- mu4e emacs email client
      , ( "M-e n"
        , openWith myEditor ["'(elfeed)'"]
        )          -- elfeed emacs rss client
      , ( "M-e s"
        , openWith myEditor ["'(eshell)'"]
        )          -- eshell within emacs
      , ( "M-e t"
        , openWith myEditor ["'(mastodon)'"]
        )        -- mastodon within emacs
        -- emms is an emacs audio player. I set it to auto start playing in a specific directory.
      , ( "M-e a"
        , openWith
            myEditor
            ["'(emms)'", "'(emms-play-directory-tree \"~/Music/Non-Classical/70s-80s/\")'"]
        )

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
      ]
    , xmonadManagement
    , windowNavigation
    , workspaceNavigation
    , windowResize
    , windowSpacing
    , layoutCommands
    , promptCommands
    ]
