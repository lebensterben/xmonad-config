-- TODO
module Custom.TreeSelect (treeSelect) where

import           Custom.Configs.Apps                      ( systemctl
                                                          , fileRoller
                                                          , qalculate
                                                          , killall
                                                          , picom
                                                          , gimp
                                                          , inkscape
                                                          , thunderbird
                                                          , alsamixer
                                                          , audacious
                                                          , vlc
                                                          , evince
                                                          , iPython
                                                          , jupyterLab
                                                          , rstudio
                                                          , vscode
                                                          , zeal
                                                          , glances
                                                          , htop
                                                          , clForum
                                                          , clRepo
                                                          , lxer
                                                          , xmonadHP
                                                          , xmonadRepo
                                                          , xmonadContribHP
                                                          , xmonadContribRepo
                                                          , xmobarHP
                                                          , emacsHP
                                                          , spacemacsRepo
                                                          , spacemacsLayers
                                                          , emacsReddit
                                                          , emacsWiki
                                                          , bashDocs
                                                          , learnShell
                                                          , elispDoc
                                                          , hoogle
                                                          , haskellReddit
                                                          , haskellStackExchange
                                                          )
import           Custom.Configs.TreeSelectConfig          ( treeSelectConfig )
import           Custom.Prompt                            ( confirmPrompt )
import           Custom.Util.TreeSelect                   ( tsTree
                                                          , tsSubTitle
                                                          , AsTSNodeArgs(asTSNodeArgs)
                                                          , AsTSNode(asTSNode)
                                                          , (>*>>)
                                                          , (>:=>)
                                                          , (>$>)
                                                          , tsSubForrest
                                                          , tsSubTree
                                                          , tsSeparator
                                                          )
import           Custom.Variables                         ( myBrowser
                                                          , myEditor
                                                          )
import           Data.Tree                                ( Tree(Node) )
import           System.Exit                              ( exitSuccess )
import           XMonad                                   ( recompile
                                                          , XConfig
                                                          , X
                                                          , io
                                                          )
import           XMonad.Actions.TreeSelect                ( treeselectAction
                                                          , TSNode(TSNode)
                                                          )
import           XMonad.Util.Run                          ( safeSpawn )


-- TODO: Screenshots
treeSelect :: XConfig l -> X ()
treeSelect conf = treeselectAction
    (treeSelectConfig conf)
    [ accessories
    , graphics
    , internet
    , multimedia
    , office
    , programming
    , system
    , tsSeparator 35
    , bookmarks
    , cfgFiles
    , tsSeparator 35
    , xmonadCtl
    , powerCtl
    ]

accessories :: Tree (TSNode (X ()))
accessories = tsSubTree
    "+ Accessories"
    "Accessory applications"
    [asTSNode fileRoller, asTSNode qalculate, asTSNodeArgs killall ["picom"] >*>> asTSNode picom]

graphics :: Tree (TSNode (X ()))
graphics = tsSubTree "+ Graphics" "graphics programs" [asTSNode gimp, asTSNode inkscape]

internet :: Tree (TSNode (X ()))
internet =
    tsSubTree "+ Internet" "internet and web programs" [asTSNode myBrowser, asTSNode thunderbird]

multimedia :: Tree (TSNode (X ()))
multimedia = tsSubTree "+ Multimedia"
                       "sound and video applications"
                       [asTSNode alsamixer, asTSNode audacious, asTSNode vlc]

office :: Tree (TSNode (X ()))
office = tsSubTree "+ Office" "office applications" [asTSNode evince]

programming :: Tree (TSNode (X ()))
programming = tsSubForrest
    "+ Programming"
    "programming and scripting tools"
    [emacs]
    [asTSNode iPython, asTSNode jupyterLab, asTSNode rstudio, asTSNode vscode, asTSNode zeal]

emacs :: Tree (TSNode (X ()))
emacs = tsSubTree
    "+ Emacs"
    "Emacs is more than a text editor"
    [ asTSNode myEditor
    , asTSNodeArgs myEditor ["(dired nil)"] >:=> ("M-x dired", "File manager for Emacs")
    , asTSNodeArgs myEditor ["(elfeed)"] >:=> ("M-x elfeed", "RSS client for Emacs") -- FIXME
    , asTSNodeArgs
            myEditor
            ["(emms)", "(emms-play-directory-tree \"/home/lucius/Music/Non-Classical/70s-80s/\")"]
        >:=> ("M-x emms", "Emacs") -- FIXME
    , asTSNodeArgs myEditor ["(erc)"] >:=> ("M-x erc", "IRC client for Emacs") -- FIXME
    , asTSNodeArgs myEditor ["(ibuffer)"] >:=> ("M-x ibuffer", "Emacs buffer list")
    , asTSNodeArgs myEditor ["(mu4e)"] >:=> ("M-x mu4e", "Email client for Emacs") -- FIXME
    ]

system :: Tree (TSNode (X ()))
system = tsSubTree "+ System" "system tools and utilities" [asTSNode glances, asTSNode htop]


-- TODO: custom start page "file:///home/lucius/.surf/html/homepage.html"
bookmarks :: Tree (TSNode (X ()))
bookmarks =
    tsSubForrest "+ Bookmarks" "a list of web bookmarks" [bmLinux, bmEmacs, bmProgramming] []

bmLinux :: Tree (TSNode (X ()))
bmLinux = tsSubForrest "+ Linux" "a list of web bookmarks" [bmClearlinux, bmLinuxNews, bmXMonad] []

bmClearlinux :: Tree (TSNode (X ()))
bmClearlinux = tsSubTree "+ Clear Linux" "Clear Linux related" [asTSNode clForum, asTSNode clRepo]

bmLinuxNews :: Tree (TSNode (X ()))
bmLinuxNews = tsSubTree "+ Linux News" "linux news and blogs" [asTSNode lxer]

bmXMonad :: Tree (TSNode (X ()))
bmXMonad = tsSubTree
    "+ Xmonad"
    "window manager documentation"
    [ asTSNode xmonadHP
    , asTSNode xmonadRepo
    , asTSNode xmonadContribHP
    , asTSNode xmonadContribRepo
    , asTSNode xmobarHP
    ]

bmEmacs :: Tree (TSNode (X ()))
bmEmacs = tsSubTree
    "+ Emacs"
    "Emacs documentation"
    [ asTSNode emacsHP
    , asTSNode spacemacsRepo
    , asTSNode spacemacsLayers
    , asTSNode emacsReddit
    , asTSNode emacsWiki
    ]

bmProgramming :: Tree (TSNode (X ()))
bmProgramming =
    tsSubForrest "+ Programming" "programming and scripting" [bmBash, bmElisp, bmHaskell] []

bmBash :: Tree (TSNode (X ()))
bmBash = tsSubTree "+ Bash and Shell Scripting"
                   "shell scripting documentation"
                   [asTSNode bashDocs, asTSNode learnShell]

bmElisp :: Tree (TSNode (X ()))
bmElisp = tsSubTree "+ Elisp" "emacs lisp documentation" [asTSNode elispDoc]

bmHaskell :: Tree (TSNode (X ()))
bmHaskell = tsSubTree "+ Haskell"
                      "haskell documentation"
                      [asTSNode hoogle, asTSNode haskellReddit, asTSNode haskellStackExchange]

cfgFiles :: Tree (TSNode (X ()))
cfgFiles = tsSubForrest
    "+ Config Files"
    "config files that edit often"
    [cfgXMonad, cfgXMobar]
    [ asTSNodeArgs myEditor ["/home/lucius/.spacemacs"] >:=> ("Spacemacs config", ".spacemacs")
    , asTSNodeArgs myEditor ["/home/lucius/.bashrc"] >:=> ("bashrc", "the bourne again shell")
    , asTSNodeArgs myEditor ["/home/lucius/.config/dunst/dunstrc"] -- FIXME
        >:=> ("dunst", "dunst notifications")
    , asTSNodeArgs myEditor ["/home/lucius/.config/kitty/kitty.conf"]
        >:=> ("kitty", "kitty terminal emulator")
    , asTSNodeArgs myEditor ["/home/lucius/.SpaceVim.d/init.toml"]
        >:=> ("SpaceVim init.vim", "SpaceVim text editor")
    , asTSNodeArgs myEditor ["/home/lucius/.config/sxhkd/sxhkdrc"] -- FIXME
        >:=> ("sxhkdrc", "simple X hotkey daemon")
    , asTSNodeArgs myEditor ["/home/lucius/tabbed-distrotube/config.h"] -- FIXME
        >:=> ("tabbed config.h", "generic tabbed interface")
    , asTSNodeArgs myEditor ["/home/lucius/.Xresources"] -- FIXME
                                                         >:=> ("xresources", "xresources file")
    , asTSNodeArgs myEditor ["/home/lucius/.zshrc"] >:=> ("zshrc", "Config for the z shell")
    , asTSNodeArgs myEditor ["/home/lucius/.zshenv"] >:=> ("zshenv", "Config for the z shell")
    , asTSNodeArgs myEditor ["/home/lucius/.zprofile"] >:=> ("zprofile", "Config for the z shell")
    ]

-- FIXME: scratchpad
cfgXMonad :: Tree (TSNode (X ()))
cfgXMonad = tsSubTree
    "+ xmonad configs"
    "My xmonad config files"
    [ asTSNodeArgs myEditor ["/home/lucius/.xmonad/xmonad.hs"] >:=> ("xmonad.hs", "My XMonad Main")
    , asTSNodeArgs myEditor ["/home/lucius/.xmonad/lib/Custom/MyGridMenu.hs"]
        >:=> ("MyGridMenu.hs", "My XMonad GridSelect menu")
    , asTSNodeArgs myEditor ["/home/lucius/.xmonad/lib/Custom/MyKeys.hs"]
        >:=> ("MyKeys.hs", "My XMonad keybindings")
    , asTSNodeArgs myEditor ["/home/lucius/.xmonad/lib/Custom/MyLayouts.hs"]
        >:=> ("MyLayouts.hs", "My XMonad layouts")
    , asTSNodeArgs myEditor ["/home/lucius/.xmonad/lib/Custom/MyPrompts.hs"]
        >:=> ("MyPrompts.hs", "My XMonad prompts")
    , asTSNodeArgs myEditor ["/home/lucius/.xmonad/lib/Custom/MyTreeSelect.hs"]
        >:=> ("MyTreeSelect.hs", "My XMonad TreeSelect menu")
    , asTSNodeArgs myEditor ["/home/lucius/.xmonad/lib/Custom/MyVariables.hs"]
        >:=> ("MyVariables.hs", "My XMonad variables")
    ]

cfgXMobar :: Tree (TSNode (X ()))
cfgXMobar = tsSubTree
    "+ xmobar configs"
    "My xmobar config files"
    [ asTSNodeArgs myEditor ["/home/lucius/.config/xmobar/xmobarrc"]
          >:=> ("xmobar mon0", "status bar on monitor 0")
    ]

-- FIXME
xmonadCtl :: Tree (TSNode (X ()))
xmonadCtl = Node
    (tsSubTitle "+ XMonad" "window manager commands")
    [ tsTree
        (TSNode "Restart"
                "Restart XMonad and recompile when necessary"
                (recompile False >> safeSpawn "xmonad" ["--restart"])
        )
    , tsTree
        (TSNode "Force Recompile and Restart"
                "Restart XMonad and recomile itself regardless"
                (recompile True >> safeSpawn "xmonad" ["--restart"])
        )
    , tsTree (TSNode "Quit" "Restart XMonad" (confirmPrompt "Exit?" $ io exitSuccess))
    ]

-- FIXME
powerCtl :: Tree (TSNode (X ()))
powerCtl = Node
    (tsSubTitle "+ Power Control" "power control commands")
    [ tsTree
        (    confirmPrompt "Suspend?"
        >$>  asTSNodeArgs systemctl ["suspend"]
        >:=> ("suspend", "suspend the system")
        )
    , tsTree
        (    confirmPrompt "Shutdown?"
        >$>  asTSNodeArgs systemctl ["shutdown"]
        >:=> ("shutdown", "shutdown the system")
        )
    , tsTree
        (    confirmPrompt "Reboot?"
        >$>  asTSNodeArgs systemctl ["Reboot"]
        >:=> ("reboot", "reboot the system")
        )
    ]
