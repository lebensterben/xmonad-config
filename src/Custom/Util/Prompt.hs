----------------------------------------------------------------------------------------------------
-- |
-- Module      : Custom.Prompt
-- Copyright   : (c) Lucius Hu, 2020-2022
-- License     : BSD3
-- Maintainer  : Lucius Hu <lebensterben@users.noreply.github.com>
--
-- Provides interfaces for various kinds of prompts.
----------------------------------------------------------------------------------------------------

module Custom.Util.Prompt (searchWithInput, searchWithSelection) where

import           Custom.Util.Run                          ( (>-$@) )
import           Data.Char                                ( toLower
                                                          , toUpper
                                                          )
import           XMonad                                   ( X )
import           XMonad.Util.Run                          ( inProgram
                                                          , proc
                                                          )
import           XMonad.Util.XSelection                   ( transformSafePromptSelection )


----------------------------------------------------------------------------------------------------
-- Prompts
----------------------------------------------------------------------------------------------------

-- | Capitalize first character in each word of a sentence.
toPascalCase :: String -- ^ String to be converted.
             -> String
toPascalCase s = unwords . map capitalizeFirst $ words s
  where
    capitalizeFirst (c : cs) = toUpper c : map toLower cs
    capitalizeFirst []       = []


-- | A list of search actions via each search engine, and a key assinged to it.
searchWithInput :: [(String, String, [String])] -- ^ A list of key-name-args.
                                                -- Key is the suffix key used in a key binding.
                                                -- Name is the name of the search provider.
                                                -- Args are arguments passed to \"rofi-surfraw\".
                -> [(String, String, X ())]
searchWithInput searchProviders =
    [ (k, desc, proc $ inProgram "rofi-surfraw" >-$@ pure (desc : args))
    | (k, name, args) <- searchProviders
    , let desc = "Search on " ++ if name == "" then toPascalCase $ head args else name
    ]

-- | Similar to 'searchWithInput' excpet that the query is the current selection.
searchWithSelection :: [(String, String, [String])] -- ^ A list of key-name-args.
                                                    -- Key is the suffix key used in a key binding.
                                                    -- Name is the name of the search provider.
                                                    -- Args are arguments passed to \"rofi-surfraw\".
                    -> [(String, String, X ())]
searchWithSelection searchProviders =
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
