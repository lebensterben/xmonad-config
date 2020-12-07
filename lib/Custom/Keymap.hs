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
      mkNamedKeymapSection
    , installNamedMajorKeys

      -- * Utility function for displaying global bindings
    , showKeyBindings

      -- * Utility function for adding tree select bindings
    , additionalTSKeysP
    )
where

import           Control.Arrow                            ( Arrow(first) )
import           Data.Foldable                            ( foldl' )
import qualified Data.Map                                as M
import           Data.Maybe                               ( listToMaybe
                                                          , mapMaybe
                                                          )
import           Text.ParserCombinators.ReadP             ( (+++)
                                                          , char
                                                          , many
                                                          , readP_to_S
                                                          , satisfy
                                                          , string
                                                          , ReadP
                                                          )
import           XMonad                                   ( X
                                                          , KeySym
                                                          , XConfig(modMask)
                                                          , KeyMask
                                                          , Layout
                                                          , controlMask
                                                          , mod1Mask
                                                          , mod2Mask
                                                          , mod3Mask
                                                          , mod4Mask
                                                          , mod5Mask
                                                          , shiftMask
                                                          , xK_F1
                                                          , io
                                                          , (.|.)
                                                          )
import qualified XMonad.Actions.TreeSelect               as TS
import           XMonad.Util.EZConfig                     ( mkNamedKeymap
                                                          , parseKey
                                                          )
import           XMonad.Util.NamedActions                 ( noName
                                                          , separator
                                                          , subtitle
                                                          , showKm
                                                          , addName
                                                          , NamedAction
                                                          , addDescrKeys'
                                                          )
import           XMonad.Util.Run                          ( safeSpawn )

----------------------------------------------------------------------------------------------------
-- Utility function for installing global bindings
----------------------------------------------------------------------------------------------------

-- | Generates a named keybindings section with descriptions.
--
-- This essentially combines the functionality of 'XMonad.Util.EZConfig.mkNamedKeymap', 'subtitle',
-- and 'separator'.
mkNamedKeymapSection :: String                   -- ^ An optional subtitle of this section.
                     -> [(String, String, X ())] -- ^ A list of binding-description-action tripples.
                     -> XConfig l                -- ^ A 'XConfig' used to determine proper modifie
                                                 --   key.
                     -> [((KeyMask, KeySym), NamedAction)]
mkNamedKeymapSection st ks conf | st == ""  = mkNamedKeymap conf keymap
                                | otherwise = subtitle st : separator : mkNamedKeymap conf keymap
    where keymap = map (\(k, desc, x) -> (k, if desc == "" then noName x else addName desc x)) ks

-- | Replace the keybindings in a 'XConfig' with the supplied one.
installNamedMajorKeys :: XConfig l -- ^ The original 'XConfig' that is modified upon.
                      -> (XConfig Layout -> [((KeyMask, KeySym), NamedAction)])
                                   -- ^ A list of binding-description-action tripples
                      -> XConfig l
installNamedMajorKeys conf keyList =
    addDescrKeys' ((modMask conf, xK_F1), showKeyBindings) keyList conf

----------------------------------------------------------------------------------------------------
-- Utility function for displaying global bindings
----------------------------------------------------------------------------------------------------

-- | Display all available global key bindings.
showKeyBindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeyBindings x = addName "Show Keybindings" $ io $ do
    _ <- safeSpawn
        "gxmessage"
        [ "-default"
        , "okay"
        , "-name"
        , "F1 Keybindings"
        , "-fn"
        , "Iosevka SS08 16"
        , unlines $ showKm x
        ]
    return ()

----------------------------------------------------------------------------------------------------
-- Utility function for adding keybindings to tree-select navigation
----------------------------------------------------------------------------------------------------

-- | Like 'XMonad.Util.EZConfig.additionalKeysP', except this takes one extra argument
-- 'TS.TSConfig', and each key binding can only contain one key sequence.
additionalTSKeysP conf tsconf keyList =
    tsconf { TS.ts_navigate = M.union (mkTSKeymap conf keyList) (TS.ts_navigate tsconf) }

-- | Similar to 'XMonad.Util.EZConfig.mkKeymap', but only allow one modifier-key combination
-- for each key-binding.
mkTSKeymap :: XConfig l -> [(String, a)] -> M.Map (KeyMask, KeySym) a
mkTSKeymap c = M.fromList . readTSKeymap c

-- | Given a configuration record and a list of (modifier-key combination
-- description, action) pairs, parse the modifier-key combos into a
-- @(KeyMask,KeySym)@ pairs.  Key sequences which fail to parse will
-- be ignored.
readTSKeymap :: XConfig l -> [(String, b)] -> [((KeyMask, KeySym), b)]
readTSKeymap c = mapMaybe (maybeKeys . first (readKeyCombo c))
  where
    maybeKeys (Nothing, _  ) = Nothing
    maybeKeys (Just k , act) = Just (k, act)

-- | Parse a modifier-key combination, returning Nothing if there is
-- a parse failure (no parse, or ambiguous parse).
readKeyCombo :: XConfig l -> String -> Maybe (KeyMask, KeySym)
readKeyCombo c = listToMaybe . parses
    where parses = map fst . filter (null . snd) . readP_to_S (parseKeyCombo c)

-- | Parse a modifier-key combination such as "M-C-s" (mod+ctrl+s).
parseKeyCombo :: XConfig l -> ReadP (KeyMask, KeySym)
parseKeyCombo c = do
    mods <- many $ parseModifier c
    k    <- parseKey
    return (foldl' (.|.) 0 mods, k)

-- | Parse a modifier: either M- (user-defined mod-key),
-- C- (control), S- (shift), or M#- where # is an integer
-- from 1 to 5 (mod1Mask through mod5Mask).
parseModifier :: XConfig l -> ReadP KeyMask
parseModifier c =
    (string "M-" >> return (modMask c))
        +++ (string "C-" >> return controlMask)
        +++ (string "S-" >> return shiftMask)
        +++ do
                _ <- char 'M'
                n <- satisfy (`elem` ['1' .. '5'])
                _ <- char '-'
                return $ indexMod (read [n] - 1)
    where indexMod = (!!) [mod1Mask, mod2Mask, mod3Mask, mod4Mask, mod5Mask]
                     -- On my keyboard, mod3Mask = Hyper_L, mod4Mask = Super_L
