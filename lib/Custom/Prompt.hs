----------------------------------------------------------------------------------------------------
-- |
-- Module      : Custom.Prompt
-- Copyright   : (c) Lucius Hu, 2020
-- License     : BSD3
-- Maintainer  : Lucius Hu <lebensterben@users.noreply.github.com>
--
-- Provides interfaces for various kinds of prompts.
----------------------------------------------------------------------------------------------------

module Custom.Prompt
    (
      -- * Prompts
      confirmExit
    , searchWithInput
    , searchWithSelection
    , shellPrompt
    , spawnPrompt
    )
where

import           Custom.Configs.PromptConfig              ( promptConfig
                                                          , AutoCompletion(..)
                                                          )
import           System.Exit                              ( exitSuccess )
import           XMonad                                   ( io
                                                          , MonadIO(liftIO)
                                                          , X
                                                          )
import           XMonad.Actions.Search                   as S
                                                          ( amazon
                                                          , google
                                                          , hoogle
                                                          , images
                                                          , maps
                                                          , promptSearch
                                                          , searchEngine
                                                          , selectSearch
                                                          , thesaurus
                                                          , vocabulary
                                                          , wikipedia
                                                          , youtube
                                                          , SearchEngine(..)
                                                          )
import           XMonad.Hooks.DynamicLog                  ( trim )
import           XMonad.Prompt                            ( XPConfig )
import           XMonad.Prompt.ConfirmPrompt              ( confirmPrompt )
import           XMonad.Prompt.Input                      ( (?+)
                                                          , inputPrompt
                                                          )
import           XMonad.Prompt.Man                        ( manPrompt )
import qualified XMonad.Prompt.Shell                     as PS
                                                          ( shellPrompt )
import           XMonad.Prompt.Ssh                        ( sshPrompt )
import           XMonad.Util.Run                          ( runProcessWithInput )

----------------------------------------------------------------------------------------------------
-- Prompts
----------------------------------------------------------------------------------------------------

-- | A prompt to ask confirmation to exit xmonad.
confirmExit :: X ()
confirmExit = confirmPrompt (promptConfig CompletionOff) "Exit?" $ io exitSuccess

-- | A shell prompt.
shellPrompt :: X ()
shellPrompt = PS.shellPrompt $ promptConfig CompletionOn

-- | A custom prompt for calculation with \"qalculate-gtk\"
calcPrompt :: XPConfig -> String -> X ()
calcPrompt c ans = inputPrompt c (trim ans)
    ?+ \input ->
           liftIO (runProcessWithInput "qalc" ["-c=off"] input) >>= calcPrompt c . (!! 2) . lines

-- | A list of prompts and a key press assigned to them.
--
-- * [@m@] : A prompt for manpages.
-- * [@s@] : A ssh prompt.
-- * [@=@] : A calculator prompt.
spawnPrompt :: [(String, String, X ())]
spawnPrompt = [ (k, dscr, f $ promptConfig CompletionOff) | (k, dscr, f) <- promptProviders ]
  where
    promptProviders =
        [ ("m", "Manpage Prompt", manPrompt)
        , ("s", "SSH Prompt"    , sshPrompt)
        , ("=", "Qalc Prompt"   , flip calcPrompt "qalc")
        ]

-- | A list of search and a key press assigne to them.
-- * [@a@] : Arch Wiki.
-- * [@c@] : Clearlinux-pkgs.
-- * [@d@] : Vocabulary.com.
-- * [@g@] : Google.
-- * [@h@] : Hoogle.
-- * [@i@] : Google Image.
-- * [@m@] : Google Maps.
-- * [@t@] : Thesaurus.
-- * [@y@] : Youtube.
-- * [@\$@] : Amazon.
searchProviders :: [(String, SearchEngine)]
searchProviders =
    [ ("a", archwiki)
    , ("c", clpkg)
    , ("d", S.vocabulary)
    , ("g", S.google)
    , ("h", S.hoogle)
    , ("i", S.images)
    , ("m", S.maps)
    , ("t", S.thesaurus)
    , ("w", S.wikipedia)
    , ("y", S.youtube)
    , ("$", S.amazon)
    ]
  where
    archwiki = S.searchEngine "archwiki" "https://wiki.archlinux.org/index.php?search="
    clpkg    = S.searchEngine "clearlinux-pkgs"
                              "https://github.com/clearlinux-pkgs?type=source&language=&q="

-- | A list of search actions via each search engine, and a key assinged to it.
searchWithInput :: [(String, String, X ())]
searchWithInput =
    [ (k, name, S.promptSearch (promptConfig CompletionOff) f)
    | (k, f@(SearchEngine name _)) <- searchProviders
    ]

-- | Similar to 'searchWithInput' excpet that the query is the current selection.
searchWithSelection :: [(String, String, X ())]
searchWithSelection =
    [ (k, name, S.selectSearch f) | (k, f@(SearchEngine name _)) <- searchProviders ]
