{-# LANGUAGE LambdaCase #-}
----------------------------------------------------------------------------------------------------
-- |
-- Module      : Custom.Keymap.MajorKeymap
-- Copyright   : (c) Lucius Hu, 2020-2022
-- License     : BSD3
-- Maintainer  : Lucius Hu <lebensterben@users.noreply.github.com>
--
-- Defines the keymap.
----------------------------------------------------------------------------------------------------
-- FIXME: Special keys are not shown in xmonad-keys

module Custom.Keymap (myMajorKeymap) where

import           Custom.Util.Keymap                       ( mkNamedKeymap' )
import           Custom.Util.Prompt                       ( searchWithInput
                                                          , searchWithSelection
                                                          )
import           Custom.Util.Run                          ( (>-$@) )
import           Custom.Util.Workspaces                   ( mergeToMaster
                                                          , moveToScreen
                                                          , moveToWS
                                                          , moveToWS'
                                                          , shiftToScreen
                                                          , shiftToWS
                                                          , shiftToWS'
                                                          , shrinkFrom
                                                          , smartSink
                                                          , smartSinkAll
                                                          )
import           Custom.Variables                         ( myScratchPads )
import           Data.List                                ( foldl' )
import           System.Exit                              ( exitSuccess )
import           XMonad                                   ( ChangeLayout(..)
                                                          , IncMasterN(..)
                                                          , KeyMask
                                                          , KeySym
                                                          , WorkspaceId
                                                          , X
                                                          , XConfig(..)
                                                          , io
                                                          , restart
                                                          , sendMessage
                                                          , withFocused
                                                          , withUnfocused
                                                          )
import           XMonad.Actions.CopyWindow                ( kill1 )
import           XMonad.Actions.CycleWindows              ( rotUnfocusedDown )
import           XMonad.Actions.Navigation2D              ( windowGo
                                                          , windowSwap
                                                          )
import           XMonad.Actions.RotSlaves                 ( rotAllDown )
import           XMonad.Actions.SwapPromote               ( swapHybrid' )
import           XMonad.Actions.WithAll                   ( killOthers )
import           XMonad.Hooks.ManageDocks                 ( ToggleStruts(..) )
import           XMonad.Layout.Hidden                     ( hideWindow
                                                          , popNewestHiddenWindow
                                                          , popOldestHiddenWindow
                                                          )
import           XMonad.Layout.LimitWindows               ( decreaseLimit
                                                          , increaseLimit
                                                          )
import qualified XMonad.Layout.MultiToggle               as MT
import           XMonad.Layout.MultiToggle.Instances      ( StdTransformers(..) )
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
import           XMonad.Util.Run                          ( (>-$)
                                                          , (>->)
                                                          , elispFun
                                                          , eval
                                                          , inEditor
                                                          , inProgram
                                                          , inTerm
                                                          , proc
                                                          , setXClass
                                                          )
import           XMonad.Util.Types                        ( Direction1D(..)
                                                          , Direction2D(..)
                                                          )
----------------------------------------------------------------------------------------------------
-- * Major Keymap
----------------------------------------------------------------------------------------------------

-- | A tree of key-sequence-to-named-actions pairs.
myMajorKeymap :: XConfig l -- ^ A 'XConfig' used to acquire modifier key.
              -> [((KeyMask, KeySym), NamedAction)]
myMajorKeymap conf =
    foldl' (^++^) mempty
        . map (mkNamedKeymap' conf)
        $ [ xmonadManagement
          , windowNavigation
          , windowResize
          , workspaceNavigation . filter (/= "NSP") $ workspaces conf
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
    [ ("M-x " <> suffix, desc, action)
    | (suffix, desc, action) <-
        [("M-r", "Restart XMonad", restart "xmonad" True), ("M-q", "Quit XMonad", io exitSuccess)]
    ]

-- | Window Navigation Keys
--
-- [@M-\<h, j, k, l\>@]: Move focus to the window in that direction.
-- [@M-S-\<h, j, k, l\>@]: Swap focused window with another window in that direction.
-- [@M-p@]: Promote focused window to master.
-- [@M-c@]: Cycle all windows except master.
-- [@M-S-c@]: Cycle all windows.
-- [@M-\<Tab\>@]: Window switcher. Type @\<Return>@ to switch to the window.
-- Or type @S-\<Return\>@ to bring the window to current workspace.
windowNavigation :: [(String, String, X ())]
windowNavigation = (<>)
    [ (prefix <> suffix, desc <> " " <> descDir dir, action dir)
    | (prefix, desc, action) <-
        [ ("M-"  , "Move focus"         , flip windowGo False)
        , ("M-S-", "Swap focused window", flip windowSwap False)
        ]
    , (suffix, dir) <- [("h", L), ("j", D), ("k", U), ("l", R)]
    ]
    [ ("M-<Tab>", "Window Switcher", proc $ inProgram "rofi-window-switcher")
    , ("M-p"    , "Promote focused window to master window", swapHybrid' False)
    , ("M-c"    , "Cycle non-master windows"               , rotUnfocusedDown)
    , ("M-S-c"  , "Cycle all window"                       , rotAllDown)
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
windowResize =
    [ ("M-M1-" <> suffix, "Shrink window on the " <> descDir dir, shrinkFrom dir)
    | (suffix, dir) <- [("h", L), ("l", R), ("j", D), ("k", U)]
    ]
  where
    descDir = \case
        L -> "Left"
        D -> "Bottom"
        U -> "Top"
        R -> "Right"

-- NOTE: left here
-- | Workspace Navigation Keys
--
-- [@M-C-\<h, j, k, l>@]: Shift focus to the workspace in that direction.
-- [@M-S-C-\<j, k>@]: Move focused window to the workspace in that direction.
-- [@M-C-\<h, l>@]: Shift focus to the screen in that direction.
-- [@M-S-C-\<h, l>@]: Move focused window to the screen in that direction.
-- [@M-\<1..9>@]: Jump to i-th workspace.
-- [@M-S-\<1..9>@]: Bring focused window to i-th workspace.
workspaceNavigation :: [WorkspaceId] -- ^ A list of 'WorkspaceId'.
                    -> [(String, String, X ())]
workspaceNavigation wss =
    [ (prefix <> suffix, unwords [descPre, desc, descSuf], action)
    | (prefix, descPre, descSuf, source) <-
        [ ("M-C-"  , "Go to"                  , "Workspace", moveToWS d1WSmapping)
        , ("M-S-C-", "Shift focused window to", "Workspace", shiftToWS d1WSmapping)
        , ("M-C-"  , "Go to"                  , "Screen"   , moveToScreen d1SCmapping)
        , ("M-S-C-", "Shift focused window to", "Screen"   , shiftToScreen d1SCmapping)
        , ("M-"    , "Go to"                  , "Workspace", moveToWS' wss)
        , ("M-S-"  , "Shift focused window to", "Workspace", shiftToWS' wss)
        ]
    , (suffix, desc, action)             <- source
    ]
  where
    d1WSmapping = [("j", Next), ("k", Prev)]
    d1SCmapping = [("h", Prev), ("l", Next)]

-- | Kill/Hide Windows
--
-- [@M-q@]: Kill focused window.
-- [@M-S-q@]: Kill all windows on current workspace.
killWindow :: [(String, String, X ())]
killWindow =
    [ ("M-q"  , "Kill focused window", kill1)
    , ("M-S-q", "Kill all windows except focuesd window on current WS", killOthers)
    ]

-- | Layout Commands
--
-- [@M-`@]: Switch to next layout.
-- [@M-S-`@]: Toggle float layout.
-- [@M-d@]: Demote (hide) focused window.
-- [@M-S-d@]: Demote (hide) other windows.
-- [@M-r@]: Restore newest hidden window.
-- [@M-S-r@]: Restore oldest hidden window.
-- [@M-m@]: Merge focused window with master window.
-- [@M-S-m@]: Merge all windows.
-- [@M-\[@]: Previous tab.
-- [@M-\]@]: Next tab
-- [@M-s@]: Sink focused window if floating, otherwise split it if it's tabbed.
-- [@M-S-s]: Sink all floating windows in current workspace if there's any, otherwise
-- split all windows in @Tabbed@ layout in current workspace.
-- [@M-\<F11\>@]: Toggle fullscreen layout.
-- [@M-S-\<F11\>@]: Toggle status bar.
-- [@M-\<KP_Add\/KP_Subtract\>@]: Increase\/Decrease number of master windows.
-- [@M-S-\<KP_Add\/KP_Subtract\>@]: Increase\/Decrease number of visible windows.
-- [@M-\<-, =\>@]: Decrease\/Increase window gap.
-- [@M-S-\<-, =\>@]: Decrease\/Increase screen gap.
layoutCmds :: [(String, String, X ())]
layoutCmds =
    [ ("M-`"              , "Cycle layouts"               , sendMessage NextLayout)
    , ("M-S-`"            , "Toggles 'floats' layout"     , sendMessage (T.Toggle "Float"))
    , ("M-d"              , "Demote (hide) focused"       , withFocused hideWindow)
    , ("M-S-d"            , "Demote (hide) other window"  , withUnfocused hideWindow)
    , ("M-r"              , "Recover newest hidden window", popNewestHiddenWindow)
    , ("M-S-r"            , "Recover oldest hidden window", popOldestHiddenWindow)
    , ("M-m"              , "Merge focused with master"   , mergeToMaster)
    , ("M-S-m"            , "Merge all"                   , withFocused (sendMessage . MergeAll))
    , ("M-s"              , "Sink/Split focused"          , smartSink)
    , ("M-S-s"            , "Sink/Split all"              , smartSinkAll)
    , ("M-["              , "Previous tab"                , onGroup W.focusUp')
    , ("M-]"              , "Next tab"                    , onGroup W.focusDown')
    , ("M-<F11>"          , "Toggles fullscreen layout"   , sendMessage (MT.Toggle NBFULL))
    , ("M-S-<F11>"        , "Toggles status bar"          , sendMessage ToggleStruts)
    , ("M-<KP_Add>"       , "More master windows"         , sendMessage (IncMasterN 1))
    , ("M-<KP_Subtract>"  , "Fewer master windows"        , sendMessage (IncMasterN (-1)))
    , ("M-S-<KP_Add>"     , "More visible windows"        , increaseLimit)
    , ("M-S-<KP_Subtract>", "Fewer visible windows"       , decreaseLimit)
    , ("M--"              , "Increase window gap"         , decWindowSpacing 4)
    , ("M-="              , "Decrease window gap"         , incWindowSpacing 4)
    , ("M-S--"            , "Increase screen gap"         , decScreenSpacing 4)
    , ("M-S-="            , "Decrease screen gap"         , incScreenSpacing 4)
    ]

-- NOTE: rofi-surfraw DISPLAYNAME ENGINE [OPTION ..] [QUERY ..]
-- TODO: add Github etc
-- | A list of searcher.
--
-- Each entry is composed of a key, a name, and a list of args:
-- * Key is the suffix key used in a key binding.
-- * Name is the name of the search provider.
-- * Args are arguments passed to \"rofi-surfraw\".
--
-- [@a@] : Arch Linux Wiki.
-- [@S-a@] : Arch Linux Package.
-- [@M1-a@] : Arch User Repository.
-- [@b@] : Gutenburg.
-- [@d@] : DuckDuckGo.
-- [@g@] : Google.
-- [@S-g@] : GitHub.
-- [@h@] : Hoogle.
-- [@i@] : Google Image.
-- [@m@] : Google Maps.
-- [@n@] : nLab.
-- [@s@] : Google Scholar.
-- [@t@] : Thesaurus.
-- [@u@] : Urban dictionary.
-- [@w@] : Wikipedia.
-- [@S-w@] : Wiktionary.
-- [@y@] : Youtube.
-- [@z@] : Wikipedia (zh).
-- [@S-z@] : Wiktionary (zh).
-- [@\$@] : Amazon.
-- [@\/@] : Choose a search engine.
searchProviders :: [(String, String, [String])]
searchProviders =
    [ ("a"   , "Arch Linux Wiki"          , ["archwiki"])
    , ("S-a" , "Arch Linux Package"       , ["archpkg"])
    , ("M1-a", "AUR"                      , ["aur"])
    , ("b"   , "Gutenburg (Book)"         , ["Gutenberg"])
    , ("d"   , "DuckDuckGo"               , ["duckduckgo"])
    , ("e"   , "Etymology"                , ["Etym"])
    , ("g"   , ""                         , ["google"])
    , ("S-g" , "Github"                   , ["github"])
    , ("h"   , ""                         , ["hoogle"])
    , ("i"   , "Google Image"             , ["google", "-i"])
    , ("m"   , "Google Maps"              , ["google", "-m"])
    , ("n"   , "nLab (Math, Physics, Philosophy)", ["nlab"])
    , ("s"   , "Google Scholar"           , ["scholar"])
    , ("t"   , ""                         , ["Thesaurus"])
    , ("u"   , "Urban Dictionary"         , ["urban"])
    , ("w"   , ""                         , ["wikipedia"])
    , ("S-w" , ""                         , ["wiktionary"])
    , ("y"   , ""                         , ["youtube"])
    , ("z"   , "Wikipedia (zh)"           , ["wikipedia", "-l=zh"])
    , ("S-z" , "Wiktionary (zh)"          , ["wiktionary", "-l=zh"])
    , ("$"   , ""                         , ["amazon"])
    , ("/"   , "specified search provider", [""])
    ]

-- | Spawning Prompts
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
    [ (prefix <> " " <> suffix, desc, action)
    | (prefix, source)       <-
        [ ("M-<Space>", spawnPrompt)
        , ("M-/"      , searchWithInput searchProviders)
        , ("M-S-/"    , searchWithSelection searchProviders)
        ]
    , (suffix, desc, action) <- source
    ]
  where
    spawnPrompt =
        [ ("m", "Manpage Prompt"       , proc $ inProgram "rofi-man")
        , ("p", "Password-store Prompt", proc $ inProgram "rofi-pass")
        , ("=", "Qalc Prompt"          , proc $ inProgram "rofi-qalc")
        ]


-- TODO: Still expanding
-- | Quick Launch
--
-- [@M-\<Return\>@]: Open terminal pop-up.
-- [@M-S-\<Return\>@]: Open terminal.
-- [@M-\<Esc\>@]: Open Zeal pop-up.
-- [@M-S-\<Esc\>@]: Open Htop.
-- [@M-o@]: App Launcher.
-- [@M-f@]: Open file manager.
-- [@M-b@]: Open browser.
-- [@M-\<F13\>@]: Screenshot whole screen.
-- [@M-S-\<F13\>@]: Screenshot selected region or window.
quickLaunch :: [(String, String, X ())]
quickLaunch = (<>)
    [ ("M-S-<Return>", "Open terminal"          , proc inTerm)
    , ("M-<Return>", "Open terminal in pop-up", namedScratchpadAction myScratchPads "terminal")
    , ("M-<Esc>", "Open quickDocs pop-up", namedScratchpadAction myScratchPads "quickDocs")
    , ( "M-S-<Esc>"
      , "Open htop"
      , proc $ inTerm >-> setXClass "AlacrittyFloat" >-$@ pure
          ["-o window.dimensions.{columns=200,lines=40}", "-e=htop"]
      )
    , ("M-o", "App Launcher"     , proc $ inProgram "rofi-launcher")
    , ("M-b", "Open browser"     , proc $ inProgram "firefox")
    , ("M-f", "Open file manager", proc $ inProgram "nautilus")
    ]
    [ (key, desc, proc $ inProgram "polybar-scrot" >-$@ pure ["-v", "viewnior", flag])
    | (key, desc, flag) <-
        [ ("M-<XF86Tools>"  , "Screenshot whole screen"             , "")
        , ("M-S-<XF86Tools>", "Screenshot selected region or window", "-s")
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
    [ ("M-e " <> suffix, desc, proc $ inEditor >-> eval (elispFun args))
    | (suffix, desc, args) <-
        [ ("e", "Open Emacs"        , "")
        , ("b", "List Emacs buffers", "ibuffer")
        , ("d", "Dired"             , "dired nil")
        , ("i", "Emacs IRC"         , "erc")
        , ("m", "mu4e"              , "mu4e")
        , ("n", "Elfeed RSS"        , "elfeed")
        ]
    ]

-- | Multi Media Keys
--
-- [@\<XF86AudioLowerVolume\>@]: Lower volume by 5%.
-- [@\<XF86AudioRaiseVolume\>@]: Raise volume by 5%.
-- [@\<XF86AudioMute\>@]: Toggle volume.
-- [@\<XF86AudioNext\>@]: Next track.
-- [@\<XF86AudioPrev\>@]: Previous track.
multiMediaKeys :: [(String, String, X ())]
multiMediaKeys = (<>)
    [ (key, desc, proc $ inProgram "amixer" >-$ pure action)
    | (key, desc, action) <-
        [ ("<XF86AudioLowerVolume>", "Lower volume by 5%", "set Master 5%- unmute")
        , ("<XF86AudioRaiseVolume>", "Raise volume by 5%", "set Master 5%+ unmute")
        , ("<XF86AudioMute>"       , "Toggle Mute"       , "set Master toggle")
        ]
    ]
    [ (key, desc, proc $ inProgram "mpc" >-$ pure action)
    | (key, desc, action) <-
        [ ("<XF86AudioPrev>", "Previous track"   , "prev")
        , ("<XF86AudioPlay>", "Toggle play/pause", "toggle")
        , ("<XF86AudioNext>", "Next track"       , "next")
        ]
    ]
