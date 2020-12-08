module Custom.TreeSelect (treeSelect) where

import           Custom.Configs.TreeSelectConfig          ( treeSelectConfig )
import           Custom.Variables                         ( openWith
                                                          , myTerminal
                                                          , myBrowser
                                                          , myEditor
                                                          )
import           Data.Tree                                ( Tree(Node) )
import           System.Exit                              ( exitSuccess )
import           XMonad                                   ( XConfig
                                                          , X
                                                          , io
                                                          )
import qualified XMonad.Actions.TreeSelect               as TS
import           XMonad.Util.Run                          ( safeSpawn
                                                          , safeSpawnProg
                                                          )

treeSelect :: XConfig l -> X ()
treeSelect conf = TS.treeselectAction
    (treeSelectConfig conf)
    [ Node
        (TS.TSNode "+ Accessories" "Accessory applications" (return ()))
        [ Node
            (TS.TSNode "Archive Manager" "Tool for archived packages" $ safeSpawnProg "file-roller")
            []
        , Node
            ( TS.TSNode "Calculator" "Gui version of qalc"
            $ safeSpawn "flatpak" ["run", "io.github.Qalculate.desktop"]
            )
            []
        , Node
            (  TS.TSNode "Picom Toggle on/off" "Compositor for window managers"
            $  safeSpawn "killall" ["picom"]
            >> safeSpawnProg "/usr/local/bin/picom"
            )
            []
        ]
    , Node
        (TS.TSNode "+ Graphics" "graphics programs" (return ()))
        [ Node (TS.TSNode "Gimp" "GNU image manipulation program" $ safeSpawnProg "gimp") []
        , Node (TS.TSNode "Inkscape" "An SVG editing program" $ safeSpawnProg "inkscape") []
        ]
    , Node
        (TS.TSNode "+ Internet" "internet and web programs" (return ()))
        [ Node (TS.TSNode "Firefox" "Open source web browser" $ safeSpawnProg "firefox")          []
        , Node (TS.TSNode "Thunderbird" "Open source email client" $ safeSpawnProg "thunderbird") []
        ]
    , Node
        (TS.TSNode "+ Multimedia" "sound and video applications" (return ()))
        [ Node
            ( TS.TSNode "Alsa Mixer" "Alsa volume control utility"
            $ openWith myTerminal ["alsamixer"]
            )
            []
        , Node (TS.TSNode "Audacious" "Lightweight audio player" $ safeSpawnProg "audacious") []
        , Node (TS.TSNode "VLC" "Multimedia player and server" $ safeSpawnProg "vlc")         []
        ]
    , Node (TS.TSNode "+ Office" "office applications" (return ()))
           [Node (TS.TSNode "Evince" "PDF Viewer" $ safeSpawnProg "evince") []]
    , Node
        (TS.TSNode "+ Programming" "programming and scripting tools" (return ()))
        [ Node
            (TS.TSNode "+ Emacs" "Emacs is more than a text editor" (return ()))
            [ Node
                (TS.TSNode "Emacs Client" "Doom Emacs launched as client" $ openWith myEditor [])
                []
            , Node
              -- FIXME
                ( TS.TSNode "M-x dired" "File manager for Emacs"
                $ openWith myEditor ["'(dired nil)'"]
                )
                []
            , Node
              -- FIXME
                (TS.TSNode "M-x elfeed" "RSS client for Emacs" $ openWith myEditor ["'(elfeed)'"])
                []
            , Node
              -- FIXME
                (TS.TSNode "M-x emms" "Emacs" $ openWith
                    myEditor
                    [ "'(emms)'"
                    , "'(emms-play-directory-tree \"/home/lucius/Music/Non-Classical/70s-80s/\")'"
                    ]
                )
                []
            , Node
              -- FIXME
                   (TS.TSNode "M-x erc" "IRC client for Emacs" $ openWith myEditor ["'(erc)'"]) []
            , Node
              -- FIXME
                (TS.TSNode "M-x ibuffer" "Emacs buffer list" $ openWith myEditor ["'(ibuffer)'"])
                []
            , Node
              -- FIXME
                   (TS.TSNode "M-x mastodon" "Emacs" $ openWith myEditor ["'(mastodon)'"]) []
            , Node
              -- FIXME
                (TS.TSNode "M-x mu4e" "Email client for Emacs" $ openWith myEditor ["'(mu4e)'"])
                []
            ]
        , Node (TS.TSNode "iPython" "A better Python Shell" $ openWith myTerminal ["ipython"]) []
        , Node
            ( TS.TSNode "Jupyter Lab" "An extensible computational environment for Jupyter"
            $ safeSpawnProg "jupyter-lab"
            )
            []
        , Node (TS.TSNode "RStudio" "R Development IDE" $ safeSpawnProg "rstudio") []
        , Node (TS.TSNode "VSCode" "Code Editing" $ safeSpawnProg "vscode")        []
        , Node (TS.TSNode "Zeal" "Documentation Browser" $ safeSpawnProg "zeal")   []
        ]
    , Node
        (TS.TSNode "+ System" "system tools and utilities" (return ()))
        [ Node (TS.TSNode "Glances" "Terminal system monitor" $ openWith myTerminal ["glances"]) []
        , Node (TS.TSNode "Htop" "Terminal process viewer" $ openWith myTerminal ["htop"])       []
        ]
    , Node (TS.TSNode "---------------------------------" "" (return ())) []
    , Node
        (TS.TSNode "+ Bookmarks" "a list of web bookmarks" (return ()))
        -- FIXME
        [ Node
            (TS.TSNode "+ Linux" "a list of web bookmarks" (return ()))
            [ Node
                (TS.TSNode "+ Clear Linux" "Clear Linux related" (return ()))
                [ Node
                    ( TS.TSNode "Clear Linux Community" "Community website"
                    $ openWith myBrowser ["https://community.clearlinux.org/"]
                    )
                    []
                , Node
                    (TS.TSNode "clearlinux/distribution" "CL GitHub issue page" $ openWith
                        myBrowser
                        [ "https://github.com/clearlinux/distribution/issues?q=is%3Aissue+is%3Aopen+sort%3Aupdated-desc"
                        ]
                    )
                    []
                ]
            , Node
                (TS.TSNode "+ Linux News" "linux news and blogs" (return ()))
                [ Node
                    ( TS.TSNode "DistroWatch" "Linux distro release announcments"
                    $ openWith myBrowser ["https://distrowatch.com/"]
                    )
                    []
                , Node
                    ( TS.TSNode "LXer" "LXer linux news aggregation"
                    $ openWith myBrowser ["http://lxer.com"]
                    )
                    []
                ]
            , Node
                (TS.TSNode "+ Xmonad" "window manager documentation" (return ()))
                [ Node
                      (TS.TSNode "+ XMonad" "xmonad documentation" (return ()))
                      [ Node
                          ( TS.TSNode "XMonad" "Homepage for XMonad"
                          $ openWith myBrowser ["http://xmonad.org"]
                          )
                          []
                      , Node
                          ( TS.TSNode "XMonad GitHub" "The GitHub page for XMonad"
                          $ openWith myBrowser ["https://github.com/xmonad/xmonad"]
                          )
                          []
                      , Node
                          ( TS.TSNode "xmonad-contrib" "Third party extensions for XMonad"
                          $ openWith
                                myBrowser
                                ["https://hackage.haskell.org/package/xmonad-contrib"]
                          )
                          []
                      , Node
                          ( TS.TSNode "xmonad-ontrib GitHub" "The GitHub page for xmonad-contrib"
                          $ openWith myBrowser ["https://github.com/xmonad/xmonad-contrib"]
                          )
                          []
                      , Node
                          ( TS.TSNode "Xmobar" "Minimal text-based status bar"
                          $ openWith myBrowser ["https://hackage.haskell.org/package/xmobar"]
                          )
                          []
                      ]
                ]
            ]
        , Node
            (TS.TSNode "+ Emacs" "Emacs documentation" (return ()))
            [ Node
                ( TS.TSNode "GNU Emacs" "Extensible free/libre text editor"
                $ openWith myBrowser ["https://www.gnu.org/software/emacs/"]
                )
                []
            , Node
                ( TS.TSNode "Spacemacs" "Emacs distribution with sane defaults"
                $ openWith myBrowser ["https://github.com/syl20bnr/spacemacs/tree/develop"]
                )
                []
            , Node
                ( TS.TSNode "Spacemacs Layer List" "Configuration layers of spacemacs"
                $ openWith myBrowser ["https://develop.spacemacs.org/layers/LAYERS.html"]
                )
                []
            , Node
                ( TS.TSNode "r/emacs" "M-x emacs-reddit"
                $ openWith myBrowser ["https://www.reddit.com/r/emacs/"]
                )
                []
            , Node
                ( TS.TSNode "EmacsWiki" "EmacsWiki Site Map"
                $ openWith myBrowser ["https://www.emacswiki.org/emacs/SiteMap"]
                )
                []
            , Node
                ( TS.TSNode "Emacs StackExchange" "Q&A site for emacs"
                $ openWith myBrowser ["https://emacs.stackexchange.com/"]
                )
                []
            ]
        , Node
            (TS.TSNode "+ Search and Reference" "Search engines, indices and wikis" (return ()))
            [ Node
                ( TS.TSNode "DuckDuckGo" "Privacy-oriented search engine"
                $ openWith myBrowser ["https://duckduckgo.com/"]
                )
                []
            , Node
                ( TS.TSNode "Google" "The evil search engine"
                $ openWith myBrowser ["http://www.google.com"]
                )
                []
            , Node
                ( TS.TSNode "Thesaurus" "Lookup synonyms and antonyms"
                $ openWith myBrowser ["https://www.thesaurus.com/"]
                )
                []
            , Node
                ( TS.TSNode "Wikipedia" "The free encyclopedia"
                $ openWith myBrowser ["https://www.wikipedia.org/"]
                )
                []
            ]
        , Node
            (TS.TSNode "+ Programming" "programming and scripting" (return ()))
            [ Node
                (TS.TSNode "+ Bash and Shell Scripting" "shell scripting documentation" (return ()))
                [ Node
                    ( TS.TSNode "GNU Bash" "Documentation for bash"
                    $ openWith myBrowser ["https://www.gnu.org/software/bash/manual/"]
                    )
                    []
                , Node
                    ( TS.TSNode "r/bash" "Subreddit for bash"
                    $ openWith myBrowser ["https://www.reddit.com/r/bash/"]
                    )
                    []
                , Node
                    ( TS.TSNode "r/commandline" "Subreddit for the command line"
                    $ openWith myBrowser ["https://www.reddit.com/r/commandline/"]
                    )
                    []
                , Node
                    ( TS.TSNode "Learn Shell" "Interactive shell tutorial"
                    $ openWith myBrowser ["https://www.learnshell.org/"]
                    )
                    []
                ]
            , Node
                (TS.TSNode "+ Elisp" "emacs lisp documentation" (return ()))
                [ Node
                    ( TS.TSNode "Emacs Lisp" "Reference manual for elisp"
                    $ openWith
                          myBrowser
                          ["https://www.gnu.org/software/emacs/manual/html_node/elisp/"]
                    )
                    []
                , Node
                    ( TS.TSNode "Learn Elisp in Y Minutes" "Single webpage for elisp basics"
                    $ openWith myBrowser ["https://learnxinyminutes.com/docs/elisp/"]
                    )
                    []
                , Node
                    ( TS.TSNode "r/Lisp" "Subreddit for lisp languages"
                    $ openWith myBrowser ["https://www.reddit.com/r/lisp/"]
                    )
                    []
                ]
            , Node
                (TS.TSNode "+ Haskell" "haskell documentation" (return ()))
                [ Node
                    ( TS.TSNode "Haskell.org" "Homepage for haskell"
                    $ openWith myBrowser ["http://www.haskell.org"]
                    )
                    []
                , Node
                    ( TS.TSNode "Hoogle" "Haskell API search engine"
                    $ openWith myBrowser ["https://hoogle.haskell.org/"]
                    )
                    []
                , Node
                    ( TS.TSNode "r/haskell" "Subreddit for haskell"
                    $ openWith myBrowser ["https://www.reddit.com/r/Python/"]
                    )
                    []
                , Node
                    ( TS.TSNode "Haskell on StackExchange" "Newest haskell topics on StackExchange"
                    $ openWith myBrowser ["https://stackoverflow.com/questions/tagged/haskell"]
                    )
                    []
                ]
            , Node
                (TS.TSNode "+ Python" "python documentation" (return ()))
                [ Node
                    ( TS.TSNode "Python.org" "Homepage for python"
                    $ openWith myBrowser ["https://www.python.org/"]
                    )
                    []
                , Node
                    ( TS.TSNode "r/Python" "Subreddit for python"
                    $ openWith myBrowser ["https://www.reddit.com/r/Python/"]
                    )
                    []
                , Node
                    ( TS.TSNode "Python on StackExchange" "Newest python topics on StackExchange"
                    $ openWith myBrowser ["https://stackoverflow.com/questions/tagged/python"]
                    )
                    []
                ]
            ]
        , Node
            (TS.TSNode "+ Vim" "vim and neovim documentation" (return ()))
            [ Node
                ( TS.TSNode "Vim.org" "Vim, the ubiquitous text editor"
                $ openWith myBrowser ["https://www.vim.org/"]
                )
                []
            , Node
                ( TS.TSNode "r/Vim" "Subreddit for vim"
                $ openWith myBrowser ["https://www.reddit.com/r/vim/"]
                )
                []
            , Node
                ( TS.TSNode "Vi/m StackExchange" "Vi/m related questions"
                $ openWith myBrowser ["https://vi.stackexchange.com/"]
                )
                []
            ]
        -- FIXME
        -- , Node
        --     (TS.TSNode "My Start Page"
        --                "Custom start page for browser"
        --                $ openWith myBrowser ["file:///home/lucius/.surf/html/homepage.html"]
        --     )
        --     []
        ]
    , Node
        (TS.TSNode "+ Config Files" "config files that edit often" (return ()))
        [ Node
            ( TS.TSNode "Spacemacs config" ".spacemacs"
            $ openWith myEditor ["/home/lucius/.spacemacs"]
            )
            []
        , Node
            (TS.TSNode "+ xmobar configs" "My xmobar config files" (return ()))
            [ Node
                  ( TS.TSNode "xmobar mon0" "status bar on monitor 0"
                  $ openWith myEditor ["/home/lucius/.config/xmobar/xmobarrc"]
                  )
                  []
            ]
        , Node
            (TS.TSNode "+ xmonad configs" "My xmonad config files" (return ()))
            [ Node
                ( TS.TSNode "xmonad.hs" "My XMonad Main"
                $ openWith myEditor ["/home/lucius/.xmonad/xmonad.hs"]
                )
                []
            , Node
                ( TS.TSNode "MyGridMenu.hs" "My XMonad GridSelect menu"
                $ openWith myEditor ["/home/lucius/.xmonad/lib/Custom/MyGridMenu.hs"]
                )
                []
            , Node
                ( TS.TSNode "MyKeys.hs" "My XMonad keybindings"
                $ openWith myEditor ["/home/lucius/.xmonad/lib/Custom/MyKeys.hs"]
                )
                []
            , Node
                ( TS.TSNode "MyLayouts.hs" "My XMonad layouts"
                $ openWith myEditor ["/home/lucius/.xmonad/lib/Custom/MyLayouts.hs"]
                )
                []
            , Node
                ( TS.TSNode "MyPrompts.hs" "My XMonad prompts"
                $ openWith myEditor ["/home/lucius/.xmonad/lib/Custom/MyPrompts.hs"]
                )
                []
            -- , Node
            --     ( TS.TSNode "MyScratchpads.hs" "My XMonad named scratchpads"
            --     $ openWith
            --           myEditor
            --           ["/home/lucius/.xmonad/lib/Custom/MyScratchpads.hs"]
            --     )
            --     []
            , Node
                ( TS.TSNode "MyTreeSelect.hs" "My XMonad TreeSelect menu"
                $ openWith myEditor ["/home/lucius/.xmonad/lib/Custom/MyTreeSelect.hs"]
                )
                []
            , Node
                ( TS.TSNode "MyVariables.hs" "My XMonad variables"
                $ openWith myEditor ["/home/lucius/.xmonad/lib/Custom/MyVariables.hs"]
                )
                []
            ]
        , Node
            ( TS.TSNode "bashrc" "the bourne again shell"
            $ openWith myEditor ["/home/lucius/.bashrc"]
            )
            []
        , Node
          -- FIXME
            ( TS.TSNode "dunst" "dunst notifications"
            $ openWith myEditor ["/home/lucius/.config/dunst/dunstrc"]
            )
            []
        , Node
            ( TS.TSNode "kitty" "kitty terminal emulator"
            $ openWith myEditor ["/home/lucius/.config/kitty/kitty.conf"]
            )
            []
        , Node
            ( TS.TSNode "SpaceVim init.vim" "SpaceVim text editor"
            $ openWith myEditor ["/home/lucius/.SpaceVim.d/init.toml"]
            )
            []
        , Node
          -- FIXME
            ( TS.TSNode "sxhkdrc" "simple X hotkey daemon"
            $ openWith myEditor ["/home/lucius/.config/sxhkd/sxhkdrc"]
            )
            []
        , Node
          -- FIXME
            ( TS.TSNode "tabbed config.h" "generic tabbed interface"
            $ openWith myEditor ["/home/lucius/tabbed-distrotube/config.h"]
            )
            []
        , Node
          -- FIXME
            ( TS.TSNode "xresources" "xresources file"
            $ openWith myEditor ["/home/lucius/.Xresources"]
            )
            []
        , Node
            (TS.TSNode "zshrc" "Config for the z shell" $ openWith myEditor ["/home/lucius/.zshrc"])
            []
        , Node
            ( TS.TSNode "zshenv" "Config for the z shell"
            $ openWith myEditor ["/home/lucius/.zshenv"]
            )
            []
        , Node
            ( TS.TSNode "zprofile" "Config for the z shell"
            $ openWith myEditor ["/home/lucius/.zprofile"]
            )
            []
        ]
    , Node
      -- FIXME
        (TS.TSNode "+ Screenshots" "take a screenshot" (return ()))
        [ Node
            ( TS.TSNode "Quick fullscreen" "take screenshot immediately"
            $ safeSpawn "scrot" ["-d", "1", "/home/lucius/scrot/%Y-%m-%d-@%H-%M-%S-scrot.png"]
            )
            []
        , Node
            ( TS.TSNode "Delayed fullscreen" "take screenshot in 5 secs"
            $ safeSpawn "scrot" ["-d", "5", "/home/lucius/scrot/%Y-%m-%d-@%H-%M-%S-scrot.png"]
            )
            []
        , Node
            ( TS.TSNode "Section screenshot" "take screenshot of section"
            $ safeSpawn "scrot" ["-s", "/home/lucius/scrot/%Y-%m-%d-@%H-%M-%S-scrot.png"]
            )
            []
        ]
    , Node (TS.TSNode "---------------------------------" "" (return ())) []
    , Node
        (TS.TSNode "+ XMonad" "window manager commands" (return ()))
        [ Node
            (TS.TSNode "+ View Workspaces" "View a specific workspace" (return ()))
            [ Node
                ( TS.TSNode "View 1" "View workspace 1"
                $ safeSpawn "/home/lucius/.xmonad/xmonadctl" ["1"]
                )
                []
            , Node
                ( TS.TSNode "View 2" "View workspace 2"
                $ safeSpawn "/home/lucius/.xmonad/xmonadctl" ["3"]
                )
                []
            , Node
                ( TS.TSNode "View 3" "View workspace 3"
                $ safeSpawn "/home/lucius/.xmonad/xmonadctl" ["5"]
                )
                []
            , Node
                ( TS.TSNode "View 4" "View workspace 4"
                $ safeSpawn "/home/lucius/.xmonad/xmonadctl" ["7"]
                )
                []
            , Node
                ( TS.TSNode "View 5" "View workspace 5"
                $ safeSpawn "/home/lucius/.xmonad/xmonadctl" ["9"]
                )
                []
            , Node
                ( TS.TSNode "View 6" "View workspace 6"
                $ safeSpawn "/home/lucius/.xmonad/xmonadctl" ["11"]
                )
                []
            , Node
                ( TS.TSNode "View 7" "View workspace 7"
                $ safeSpawn "/home/lucius/.xmonad/xmonadctl" ["13"]
                )
                []
            , Node
                ( TS.TSNode "View 8" "View workspace 8"
                $ safeSpawn "/home/lucius/.xmonad/xmonadctl" ["15"]
                )
                []
            , Node
                ( TS.TSNode "View 9" "View workspace 9"
                $ safeSpawn "/home/lucius/.xmonad/xmonadctl" ["17"]
                )
                []
            ]
        , Node
            (TS.TSNode "+ Shift Workspaces" "Send focused window to specific workspace" (return ()))
            [ Node
                ( TS.TSNode "View 1" "View workspace 1"
                $ safeSpawn "/home/lucius/.xmonad/xmonadctl" ["2"]
                )
                []
            , Node
                ( TS.TSNode "View 2" "View workspace 2"
                $ safeSpawn "/home/lucius/.xmonad/xmonadctl" ["4"]
                )
                []
            , Node
                ( TS.TSNode "View 3" "View workspace 3"
                $ safeSpawn "/home/lucius/.xmonad/xmonadctl" ["6"]
                )
                []
            , Node
                ( TS.TSNode "View 4" "View workspace 4"
                $ safeSpawn "/home/lucius/.xmonad/xmonadctl" ["8"]
                )
                []
            , Node
                ( TS.TSNode "View 5" "View workspace 5"
                $ safeSpawn "/home/lucius/.xmonad/xmonadctl" ["10"]
                )
                []
            , Node
                ( TS.TSNode "View 6" "View workspace 6"
                $ safeSpawn "/home/lucius/.xmonad/xmonadctl" ["12"]
                )
                []
            , Node
                ( TS.TSNode "View 7" "View workspace 7"
                $ safeSpawn "/home/lucius/.xmonad/xmonadctl" ["14"]
                )
                []
            , Node
                ( TS.TSNode "View 8" "View workspace 8"
                $ safeSpawn "/home/lucius/.xmonad/xmonadctl" ["16"]
                )
                []
            , Node
                ( TS.TSNode "View 9" "View workspace 9"
                $ safeSpawn "/home/lucius/.xmonad/xmonadctl" ["18"]
                )
                []
            ]
        , Node
            ( TS.TSNode "Next layout" "Switch to next layout"
            $ safeSpawn "/home/lucius/.xmonad/xmonadctl" ["next-layout"]
            )
            []
        , Node (TS.TSNode "Recompile" "Recompile XMonad" $ safeSpawn "xmonad" ["--recompile"]) []
        , Node (TS.TSNode "Restart" "Restart XMonad" $ safeSpawn "xmonad" ["--restart"])       []
        , Node (TS.TSNode "Quit" "Restart XMonad" (io exitSuccess))                            []
        ]
    ]
