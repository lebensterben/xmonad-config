----------------------------------------------------------------------------------------------------
-- |
-- Module      : XMonad.Custom.Keymap
-- Copyright   : (c) Lucius Hu, 2020-2022
-- License     : BSD3
-- Maintainer  : Lucius Hu <lebensterben@users.noreply.github.com>
--
-- Provides common utilities for managing keybindings.
----------------------------------------------------------------------------------------------------

module XMonad.Custom.Util.Keymap (mkNamedKeymap', addNamedKeys) where

import           Control.Exception                        ( SomeException(..)
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
import           XMonad                                   ( Directories'(..)
                                                          , KeyMask
                                                          , KeySym
                                                          , Layout
                                                          , X
                                                          , XConf(..)
                                                          , XConfig
                                                          , asks
                                                          , io
                                                          , modMask
                                                          , xK_F1
                                                          )
import           XMonad.Hooks.DynamicLog                  ( wrap )
import           XMonad.Util.EZConfig                     ( mkNamedKeymap )
import           XMonad.Util.NamedActions                 ( NamedAction
                                                          , addDescrKeys'
                                                          , addName
                                                          , noName
                                                          , showKmSimple
                                                          )
import           XMonad.Util.Run                          ( inProgram
                                                          , proc
                                                          )

----------------------------------------------------------------------------------------------------
-- Utility function for installing global bindings
----------------------------------------------------------------------------------------------------
-- | Similar to 'addName', but adding @Pango@ format tags.
addName' :: String -- ^ Description.
         -> X ()   -- ^ Action.
         -> NamedAction
addName' = addName . wrap "<span><big>" "</big></span>"

-- | Similar to 'mkNamedKeymap' but using 'addName''.
mkNamedKeymap' :: XConfig l                -- ^ A 'XConfig' used to determine proper modifier key.
               -> [(String, String, X ())] -- ^ A list of binding-description-action tripples.
               -> [((KeyMask, KeySym), NamedAction)]
mkNamedKeymap' conf ks = mkNamedKeymap conf keymap
    where keymap = map (\(k, desc, x) -> (k, if null desc then noName x else addName' desc x)) ks

-- | Similar to 'addDescrKeys'', but using 'mkNamedKeymap''.
addNamedKeys :: (XConfig Layout -> [((KeyMask, KeySym), NamedAction)])
                          -- ^ A list of binding-description-action triples.
             -> XConfig l -- ^ The original 'XConfig' that is modified upon.
             -> XConfig l
addNamedKeys keyList conf = addDescrKeys' ((modMask conf, xK_F1), showKeyBindings) keyList conf

----------------------------------------------------------------------------------------------------
-- Utility function for displaying global bindings
----------------------------------------------------------------------------------------------------
-- | Display all available global key bindings.
showKeyBindings :: [((KeyMask, KeySym), NamedAction)] -- ^ A list of binding-description-action triples.
                -> NamedAction
showKeyBindings x = addName' "Show Keybindings" $ regenerateKeyBindings (showKmSimple x) >> proc
    (inProgram "rofi-xmonad-keys")

-- | Export key bindings to @/tmp/xmonad-keys@ when it satisfies one of the conditions:
--
-- * The @xmonad@ binary is newer than the export file.
-- * The export file doesn't exist.
regenerateKeyBindings :: [String] -- ^ Description of keyboard bindings.
                      -> X ()
regenerateKeyBindings bindings = do
    datadir <- asks (dataDir . directories)
    let bin  = (</>) datadir $ "xmonad-" ++ arch ++ "-" ++ os
        keys = "/tmp/xmonad-keys"
    binT  <- io $ getModTime bin
    keysT <- io $ getModTime keys
    let shouldRegenerate = binT > keysT
    io $ when shouldRegenerate $ writeFile keys $ processBinding bindings
  where
    getModTime f = catch (Just <$> getModificationTime f) (\(SomeException _) -> return Nothing)
    processBinding = unlines . map (fst . last . readP_to_S namedKeySequences)

-- | Translate key bindings to human readable forms.
-- Specifically, it adds @Pango@ text formats and translates modifier keys.
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
