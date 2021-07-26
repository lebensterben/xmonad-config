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
    ) where

import           Custom.Variables                         ( base8
                                                          , colorBgAlt
                                                          , myUIFont
                                                          )
import           XMonad                                   ( def )
import           XMonad.Layout.ShowWName                  ( SWNConfig(..) )

----------------------------------------------------------------------------------------------------
-- ShowWName Config
----------------------------------------------------------------------------------------------------

-- | Config for showing workspaces name.
showWNameConfig :: SWNConfig
showWNameConfig = def { swn_font    = myUIFont 60.0 "bold"
                      , swn_fade    = 1.0
                      , swn_bgcolor = colorBgAlt -- #1c1f24
                      , swn_color   = base8 -- #dfdfdf
                      }

-- Local Variables:
-- eval: (when (fboundp 'xah-syntax-color-hex) (eval '(xah-syntax-color-hex)))
-- end:
