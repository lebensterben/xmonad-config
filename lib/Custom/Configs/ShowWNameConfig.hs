----------------------------------------------------------------------------------------------------
-- |
-- Module      : Custom.Configs.ShowWNameConfig
-- Copyright   : (c) Lucius Hu, 2020
-- License     : BSD3
-- Maintainer  : Lucius Hu <lebensterben@users.noreply.github.com>
--
-- Configs used for showing Workspace/Window Name.
----------------------------------------------------------------------------------------------------

module Custom.Configs.ShowWNameConfig
    (
    -- * ShowWName Config
      showWNameConfig
    )
where

import           Custom.Util.Color                        ( ColorScheme(..) )
import           Custom.Util.Font                         ( Font(..)
                                                          , FontFace(..)
                                                          , FontWeight(..)
                                                          , FontSize(..)
                                                          )
import           Custom.Variables                         ( myColor
                                                          , myFontSet
                                                          )
import           XMonad                                   ( def )
import           XMonad.Layout.ShowWName                  ( SWNConfig(..) )

----------------------------------------------------------------------------------------------------
-- ShowWName Config
----------------------------------------------------------------------------------------------------

-- | Config for showing workspaces name.
showWNameConfig :: SWNConfig
showWNameConfig = def { swn_font    = show $ Font myFontSet UI Bold $ FontSize 60
                      , swn_fade    = 1.0           -- fade after 1 second
                      , swn_bgcolor = base1 myColor -- #1c1f24
                      , swn_color   = base8 myColor -- #dfdfdf
                      }

-- Local Variables:
-- eval: (when (fboundp 'xah-syntax-color-hex) (eval '(xah-syntax-color-hex)))
-- end:
