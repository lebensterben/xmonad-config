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
      searchWithInput
    , searchWithSelection
    , spawnPrompt
    ) where

import           Data.Char                                ( toLower
                                                          , toUpper
                                                          )
import           XMonad                                   ( X )
import           XMonad.Util.Run                          ( safeSpawn
                                                          , safeSpawnProg
                                                          )
import           XMonad.Util.XSelection                   ( transformSafePromptSelection )

----------------------------------------------------------------------------------------------------
-- Prompts
----------------------------------------------------------------------------------------------------

-- | A list of prompts and a key press assigned to them.
--
-- [@m@] : A prompt for manpages.
-- [@p@] : A password store prompt.
-- [@=@] : A calculator prompt.
spawnPrompt :: [(String, String, X ())]
spawnPrompt =
    [ ("m", "Manpage Prompt"       , safeSpawnProg "rofi-man")
    , ("p", "Password-store Prompt", safeSpawnProg "rofi-pass")
    , ("=", "Qalc Prompt"          , safeSpawnProg "rofi-qalc")
    ]

-- | A list of search and a key press assigne to them.
--
-- [@a@] : Arch Linux Wiki.
-- [@S-a@] : Arch Linux Package.
-- [@c@] : Clear Linux Package.
-- [@S-a@] : Clear Linux Bundle.
-- [@d@] : DuckDuckGo.
-- [@g@] : Google.
-- [@h@] : Hoogle.
-- [@i@] : Google Image.
-- [@m@] : Google Maps.
-- [@t@] : Thesaurus.
-- [@w@] : Wikipedia.
-- [@S-w@] : Wiktionary.
-- [@y@] : Youtube.
-- [@z@] : Wikipedia (zh).
-- [@S-z@] : Wiktionary (zh).
-- [@\$@] : Amazon.
-- [@\/@] : Choose a search engine.
searchProviders :: [(String, String, [String])]
searchProviders =
    [ ("a"  , "Arch Linux Wiki"          , ["archwiki"])
    , ("S-a", "Arch Linux Package"       , ["archpkg"])
    , ("c"  , "Clear Linux Package"      , ["clearlinux", "-p"])
    , ("S-c", "Clear Linux Bundle"       , ["clearlinux", "-b"])
    , ("d"  , "DuckDuckGo"               , ["duckduckgo"])
    , ("g"  , ""                         , ["google"])
    , ("h"  , ""                         , ["hoogle"])
    , ("i"  , "Google Image"             , ["google", "-i"])
    , ("m"  , "Google Maps"              , ["google", "-m"])
    , ("t"  , ""                         , ["Thesaurus"])
    , ("w"  , ""                         , ["wikipedia"])
    , ("S-w", ""                         , ["wiktionary"])
    , ("y"  , ""                         , ["youtube"])
    , ("z"  , "Wikipedia (zh)"           , ["wikipedia", "-l=zh"])
    , ("S-z", "Wiktionary (zh)"          , ["wiktionary", "-l=zh"])
    , ("$"  , ""                         , ["amazon"])
    , ("/"  , "specified search provider", [""])
    ]
-- NOTE: rofi-surfraw DISPLAYNAME ENGINE [OPTION ..] [QUERY ..]
-- TODO: add Github etc

-- | A list of search actions via each search engine, and a key assinged to it.
searchWithInput :: [(String, String, X ())]
searchWithInput =
    [ (k, desc, safeSpawn "rofi-surfraw" (desc : args))
    | (k, name, args) <- searchProviders
    , let desc = "Search on " ++ if name == "" then toPascalCase $ head args else name
    ]

-- | Similar to 'searchWithInput' excpet that the query is the current selection.
searchWithSelection :: [(String, String, X ())]
searchWithSelection =
    [ ( k
      , desc
      , transformSafePromptSelection
          (\query -> unwords $ desc : args ++ case query of
              "" -> ["--", query]
              _  -> []
          )
          "rofi-surfraw"
      )
    | (k, name, args) <- searchProviders
    , let desc = "Search on "
              ++ if name == "" then toPascalCase $ head args else name ++ " w/ input"
    ]

toPascalCase :: String -> String
toPascalCase s = unwords . map capitalizeFirst $ words s
  where
    capitalizeFirst (c : cs) = toUpper c : map toLower cs
    capitalizeFirst []       = []
