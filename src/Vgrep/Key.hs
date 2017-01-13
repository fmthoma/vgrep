{-# LANGUAGE DeriveGeneric #-}
-- | Basic definitions for 'Key's, 'Mod'ifiers, and 'Chord's of 'Key's and
-- 'Mod'ifiers. We can read key 'Chord's from "Graphics.Vty" 'Vty.EvKey' events
-- using 'fromVty'.
--
-- This module is intended for qualified import:
--
-- > import qualified Vgrep.Key as Key
--
-- We define our own 'Key' and 'Mod' types rather than using "Graphics.Vty"'s
-- 'Vty.Key' and 'Vty.Modifier', because it simplifies parsing (of keys like
-- 'Space' and 'Tab', which are represented as @' '@ and @'\t'@ in
-- "Graphics.Vty"), and because a 'Set' of 'Mod's is simpler to check for
-- equality than a list of 'Vty.Modifier's.
module Vgrep.Key
  ( Chord (..)
  , Key (..)
  , Mod (..)
  , fromVty
  , key
  , withModifier
  )where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
    ( FromJSON (..)
    , FromJSONKey (..)
    , FromJSONKeyFunction (..)
    )
import           Data.Monoid
import           Data.Set                  (Set)
import qualified Data.Set                  as S
import qualified Data.Text                 as T
import           GHC.Generics
import qualified Graphics.Vty.Input.Events as Vty
import           Prelude                   hiding (Left, Right)
import           Text.Read                 (readMaybe)


-- | A chord of keys and modifiers pressed simultaneously.
data Chord = Chord (Set Mod) Key
    deriving (Eq, Ord, Show, Generic)

instance FromJSON Chord where
    parseJSON = parseJSON >=> parseChord

instance FromJSONKey Chord where
    fromJSONKey = FromJSONKeyTextParser (parseChord . T.unpack)

data Key
    = Char Char | Space
    | Esc | Backspace | Enter | Del | Tab
    | Left | Right | Up     | Down
    | Home | End   | PageUp | PageDown
    deriving (Eq, Ord, Show, Read, Generic)

data Mod
    = Ctrl
    | Meta
    | Shift
    deriving (Eq, Ord, Show, Generic)

parseChord :: Monad m => String -> m Chord
parseChord = \case
    'C' : '-' : t -> fmap (`withModifier` Ctrl)  (parseChord t)
    'S' : '-' : t -> fmap (`withModifier` Shift) (parseChord t)
    'M' : '-' : t -> fmap (`withModifier` Meta)  (parseChord t)
    [c]           -> pure (key (Char c))
    "PgUp"        -> pure (key PageUp)
    "PgDown"      -> pure (key PageDown)
    "PgDn"        -> pure (key PageDown)
    s | Just k <- readMaybe s
                  -> pure (key k)
      | otherwise -> fail ("Unknown key '" <> s <> "'")

-- | Reads the key and modifiers from an 'Vty.Event'. Non-key events and events
-- with unknown keys are mapped to 'Nothing'.
fromVty :: Vty.Event -> Maybe Chord
fromVty = \case
    Vty.EvKey k ms -> liftA2 Chord (mapModifiers ms) (mapKey k)
    _otherwise     -> Nothing

mapModifiers :: [Vty.Modifier] -> Maybe (Set Mod)
mapModifiers = Just . S.fromList . map go
  where
    go = \case
        Vty.MCtrl  -> Ctrl
        Vty.MShift -> Shift
        Vty.MMeta  -> Meta
        Vty.MAlt   -> Meta

mapKey :: Vty.Key -> Maybe Key
mapKey = \case
    Vty.KChar ' '  -> Just Space
    Vty.KEsc       -> Just Esc
    Vty.KBS        -> Just Backspace
    Vty.KEnter     -> Just Enter
    Vty.KDel       -> Just Del
    Vty.KChar '\t' -> Just Tab
    Vty.KLeft      -> Just Left
    Vty.KRight     -> Just Right
    Vty.KUp        -> Just Up
    Vty.KDown      -> Just Down
    Vty.KHome      -> Just Home
    Vty.KEnd       -> Just End
    Vty.KPageUp    -> Just PageUp
    Vty.KPageDown  -> Just PageDown
    Vty.KChar c    -> Just (Char c)
    _otherwise     -> Nothing


-- | Build a 'Chord' from a single 'Key'
key :: Key -> Chord
key = Chord S.empty

-- | Add a 'Mod'ifier to a 'Chord'
withModifier :: Chord -> Mod -> Chord
withModifier (Chord ms k) m = Chord (S.insert m ms) k
