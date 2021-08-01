----------------------------------------------------------------------------------------------------
-- |
-- Module      : Custom.Keymap
-- Copyright   : (c) Lucius Hu, 2020
-- License     : BSD3
-- Maintainer  : Lucius Hu <lebensterben@users.noreply.github.com>
--
-- Defines 'modKey', and provides common utilities for the managing keybindings.
----------------------------------------------------------------------------------------------------

module Custom.Keymap
    (
      -- * Utility function for adding global bindings
      mkNamedKeymap'
    , addNamedKeys
    ) where

import           Control.Exception                        ( SomeException(SomeException)
                                                          , catch
                                                          )
import           Control.Monad                            ( when )
import           System.Directory                         ( getModificationTime )
import           System.FilePath                          ( (</>) )
import           System.Info                              ( arch
                                                          , os
                                                          )
import           Text.ParserCombinators.ReadP             ( (+++)
                                                          , ReadP
                                                          , char
                                                          , many
                                                          , many1
                                                          , manyTill
                                                          , readP_to_S
                                                          , satisfy
                                                          , string
                                                          )
import           XMonad                                   ( KeyMask
                                                          , KeySym
                                                          , Layout
                                                          , X
                                                          , XConfig
                                                          , getXMonadDataDir
                                                          , io
                                                          , modMask
                                                          , xK_F1
                                                          )
import           XMonad.Util.EZConfig                     ( mkNamedKeymap )
import           XMonad.Util.NamedActions                 ( NamedAction
                                                          , addDescrKeys'
                                                          , addName
                                                          , noName
                                                          , showKmSimple
                                                          )
import           XMonad.Util.Run                          ( safeSpawnProg )

----------------------------------------------------------------------------------------------------
-- Utility function for installing global bindings
----------------------------------------------------------------------------------------------------
addName' :: String -> X () -> NamedAction
addName' desc = addName (padDescription desc)
    where padDescription x = "<span><big>" ++ x ++ "</big></span>"

-- | Generates a named keybindings section with descriptions.
mkNamedKeymap' :: XConfig l                -- ^ A 'XConfig' used to determine proper modifie
               -> [(String, String, X ())] -- ^ A list of binding-description-action tripples.
                                                 --   key.
               -> [((KeyMask, KeySym), NamedAction)]
mkNamedKeymap' conf ks = mkNamedKeymap conf keymap
    where keymap = map (\(k, desc, x) -> (k, if null desc then noName x else addName' desc x)) ks


-- | Add keybindings with descriptions (without merging defaults).
addNamedKeys :: (XConfig Layout -> [((KeyMask, KeySym), NamedAction)])
                -- ^ A list of binding-description-action tripples
             -> XConfig l -- ^ The original 'XConfig' that is modified upon.
             -> XConfig l
addNamedKeys keyList conf = addDescrKeys' ((modMask conf, xK_F1), showKeyBindings) keyList conf

----------------------------------------------------------------------------------------------------
-- Utility function for displaying global bindings
----------------------------------------------------------------------------------------------------
-- | Display all available global key bindings.
showKeyBindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeyBindings x =
    addName' "Show Keybindings" $ io $ regenerateKeyBindings (showKmSimple x) >> safeSpawnProg
        "/home/lucius/.local/bin/rofi-xmonad-keys"

regenerateKeyBindings :: [String] -> IO ()
regenerateKeyBindings bindings = do
    datadir <- getXMonadDataDir
    let bin  = (</>) datadir $ "xmonad-" ++ arch ++ "-" ++ os
        keys = "/tmp/xmonad-keys"
    binT  <- getModTime bin
    keysT <- getModTime keys
    let shouldRegenerate = binT > keysT
    when shouldRegenerate $ writeFile keys $ processBinding bindings
  where
    getModTime f = catch (Just <$> getModificationTime f) (\(SomeException _) -> return Nothing)
    processBinding = unlines . map (fst . last . readP_to_S namedKeySequences)

namedKeySequences :: ReadP String
namedKeySequences = do
    keys <- many1 keySequence
    desc <- description
    return $ desc ++ " := <span><u>" ++ unwords keys ++ "</u></span>"
  where
    anyChar :: ReadP Char
    anyChar = satisfy $ const True

    keySequence :: ReadP String
    keySequence = do
        mods <- many modifier
        keys <- manyTill anyChar (many1 $ char ' ')
        return $ concat mods ++ keys
      where
        modifier :: ReadP String
        modifier =
            (string "M4-" >> return "Super+")
                +++ (string "M1-" >> return "Alt+")
                +++ (string "Shift-" >> return "Shift+")
                +++ (string "C-" >> return "Control+")

    description :: ReadP String
    description = do
        let open  = "<span><big>"
            close = "</big></span>"
        _    <- string open
        desc <- manyTill anyChar (string close)
        return (open ++ desc ++ close)
