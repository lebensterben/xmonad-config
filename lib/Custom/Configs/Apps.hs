-- TODO
module Custom.Configs.Apps where

import           Custom.Util.Apps                         ( simplestArgsWrapper
                                                          , App(..)
                                                          , TerminalApp(..)
                                                          , WebApp(..)
                                                          )

----------------------------------------------------------------------------------------------------
-- Apps used in various contexts
----------------------------------------------------------------------------------------------------

alsamixer :: TerminalApp
alsamixer = TerminalApp "alsamixer" "Alsa Mixer" "Alsa volume control utility" simplestArgsWrapper

audacious :: App
audacious = App "audacious" "Audacious" "Lightweight audio player" simplestArgsWrapper

evince :: App
evince = App "evince" "Evince" "PDF Viewer" simplestArgsWrapper

fileRoller :: App
fileRoller = App "file-roller" "Archive Manager" "Tool for archived packages" simplestArgsWrapper

gimp :: App
gimp = App "gimp" "Gimp" "GNU image manipulation program" simplestArgsWrapper

glances :: TerminalApp
glances = TerminalApp "glances" "Glances" "Terminal system monitor" simplestArgsWrapper

htop :: TerminalApp
htop = TerminalApp "htop" "Htop" "Terminal process viewer" simplestArgsWrapper

inkscape :: App
inkscape = App "inkscape" "Inkscape" "An SVG editing program" simplestArgsWrapper

iPython :: TerminalApp
iPython = TerminalApp "ipython" "iPython" "A better Python Shell" simplestArgsWrapper

jupyterLab :: App
jupyterLab = App "jupyter-lab"
                 "Jupyter Lab"
                 "An extensible computational environment for Jupyter"
                 simplestArgsWrapper

killall :: App
killall = App "killall" "Kill process by name" "Kill process by name" simplestArgsWrapper

picom :: App
picom = App "/usr/local/bin/picom"
            "Picom Toggle on/off"
            "Compositor for window manager"
            simplestArgsWrapper

qalculate :: App
qalculate = App "qalculate-gtk" "Calculator" "Gui version of qalc" simplestArgsWrapper

rstudio :: App
rstudio = App "rstudio" "RStudio" "R Development IDE" simplestArgsWrapper

systemctl :: App
systemctl =
    App "systemctl" "systemctl" "Control the systemd system and service manager" simplestArgsWrapper

thunderbird :: App
thunderbird = App "thunderbird" "Thunderbird" "Open source email client" simplestArgsWrapper

vlc :: App
vlc = App "vlc" "VLC" "Multimedia player and server" simplestArgsWrapper

vscode :: App
vscode = App "vscode" "VSCode" "Code Editing" simplestArgsWrapper

zeal :: App
zeal = App "zeal" "Zeal" "Documentation Browser" simplestArgsWrapper

----------------------------------------------------------------------------------------------------
-- Web Apps or Bookmarks
----------------------------------------------------------------------------------------------------

bashDocs :: WebApp
bashDocs = WebApp "https://www.gnu.org/software/bash/manual/" "GNU Bash" "Documentation for bash"

clForum :: WebApp
clForum = WebApp "https://community.clearlinux.org/" "Clear Linux Community" "Community website"

clRepo :: WebApp
clRepo = WebApp
    "https://github.com/clearlinux/distribution/issues?q=is%3Aissue+is%3Aopen+sort%3Aupdated-desc"
    "clearlinux/distribution"
    "CL GitHub issue page"

elispDoc :: WebApp
elispDoc = WebApp "https://www.gnu.org/software/emacs/manual/html_node/elisp/"
                  "Emacs Lisp"
                  "Reference manual for elisp"

emacsHP :: WebApp
emacsHP =
    WebApp "https://www.gnu.org/software/emacs/" "GNU Emacs" "Extensible free/libre text editor"

emacsReddit :: WebApp
emacsReddit = WebApp "https://www.reddit.com/r/emacs/" "r/emacs" "M-x emacs-reddit"

emacsWiki :: WebApp
emacsWiki = WebApp "https://www.emacswiki.org/emacs/SiteMap" "EmacsWiki" "EmacsWiki Site Map"

haskellReddit :: WebApp
haskellReddit = WebApp "https://www.reddit.com/r/haskell/" "r/haskell" "Subreddit for haskell"

haskellStackExchange :: WebApp
haskellStackExchange = WebApp "https://stackoverflow.com/questions/tagged/haskell"
                              "Haskell on StackExchange"
                              "Newest haskell topics on StackExchange"

hoogle :: WebApp
hoogle = WebApp "https://hoogle.haskell.org/" "Hoogle" "Haskell API search engine"

learnShell :: WebApp
learnShell = WebApp "https://www.learnshell.org/" "Learn Shell" "Interactive shell tutorial"

lxer :: WebApp
lxer = WebApp "http://lxer.com" "LXer" "LXer linux news aggregation"

spacemacsLayers :: WebApp
spacemacsLayers = WebApp "https://develop.spacemacs.org/layers/LAYERS.html"
                         "Spacemacs Layer List"
                         "Configuration layers of spacemacs"

spacemacsRepo :: WebApp
spacemacsRepo = WebApp "https://github.com/syl20bnr/spacemacs/tree/develop"
                       "Spacemacs"
                       "Emacs distribution with sane defaults"

xmobarHP :: WebApp
xmobarHP =
    WebApp "https://hackage.haskell.org/package/xmobar" "Xmobar" "Minimal text-based status bar"

xmonadHP :: WebApp
xmonadHP = WebApp "http://xmonad.org" "XMonad" "Homepage for XMonad"

xmonadRepo :: WebApp
xmonadRepo = WebApp "https://github.com/xmonad/xmonad" "XMonad GitHub" "The GitHub page for XMonad"

xmonadContribHP :: WebApp
xmonadContribHP = WebApp "https://hackage.haskell.org/package/xmonad-contrib"
                         "xmonad-contrib"
                         "Third party extensions for XMonad"

xmonadContribRepo :: WebApp
xmonadContribRepo = WebApp "https://github.com/xmonad/xmonad-contrib"
                           "xmonad-ontrib GitHub"
                           "The GitHub page for xmonad-contrib"
