----------------------------------------------------------------------------------------------------
-- |
-- Module      : Custom.Configs.PromptConfig
-- Copyright   : (c) Lucius Hu, 2020
-- License     : BSD3
-- Maintainer  : Lucius Hu <lebensterben@users.noreply.github.com>
--
-- Configuration for prompts.
----------------------------------------------------------------------------------------------------

module Custom.Configs.PromptConfig
    (
    -- * Prompt Config
      promptConfig
    , AutoCompletion(..)
    )
where

import           Custom.Variables                         ( myBorderWidth
                                                          , myColor
                                                          , myFontSet
                                                          )
import           Custom.Util.Color                        ( ColorScheme(..) )
import           Custom.Util.Font                         ( Font(..)
                                                          , FontFace(..)
                                                          , FontWeight(..)
                                                          , FontSize(..)
                                                          )
import           XMonad                                   ( def )
import           XMonad.Prompt                            ( emacsLikeXPKeymap
                                                          , XPConfig(..)
                                                          , XPPosition(..)
                                                          )
import           XMonad.Prompt.FuzzyMatch                 ( fuzzyMatch )

----------------------------------------------------------------------------------------------------
-- Prompt Config
----------------------------------------------------------------------------------------------------

-- | Represents whether auto-completion should be turned on for a 'XPConfig'.
data AutoCompletion = CompletionOn | CompletionOff

-- | The config for prompts.
-- Notably, this uses emacs-style keybindings:
--
-- [@C-\<u,k>@]: Kill Before/After
-- [@C-\<a,e>@]: Start/End of line
-- [@C-\<b,f>@]: Move cursor forward/backword.
-- [@C-d@]: Delete next character.
-- [@C-\<Backspace>@]: Delete last word.
-- [@C-y@]: Paste.
-- [@C-\<g,{>@]: Quit.
promptConfig :: AutoCompletion -- ^ * When 'CompletionOn', the prompt enables auto-completion.
                               --   * When 'CompletionOff', auto-completion is disabled, which
                               --     would be annoying for scenarios like search engine prompt.
             -> XPConfig
promptConfig comp = def
    { font              = show $ Font myFontSet Term DemiBold $ FontSize 13
    , bgColor           = bg myColor         -- #282c34
    , fgColor           = fg myColor         -- #bbc2cf
    , bgHLight          = darkBlue myColor   -- #2257a0
    , fgHLight          = base8 myColor      -- #dfdfdf
    , promptBorderWidth = myBorderWidth
    , position          = Top
    , alwaysHighlight   = True
    , height            = 20                 -- The same as those in xmobarrc
    , promptKeymap      = emacsLikeXPKeymap
    , autoComplete      = case comp of
                              CompletionOn -> Just 100000
                              _            -> Nothing
    , searchPredicate   = fuzzyMatch
    }

-- Local Variables:
-- eval: (when (fboundp 'xah-syntax-color-hex) (eval '(xah-syntax-color-hex)))
-- end:
