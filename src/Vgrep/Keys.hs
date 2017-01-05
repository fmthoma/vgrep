{-# LANGUAGE DeriveGeneric #-}
module Vgrep.Keys where

import Prelude hiding (Left, Right)
import           Control.Applicative
import           Data.Set                  (Set)
import qualified Data.Set                  as S
import           GHC.Generics
import qualified Graphics.Vty.Input.Events as Vty


data Chord = Chord (Set Mod) Key
    deriving (Eq, Ord, Show, Generic)

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

withModifier :: Chord -> Mod -> Chord
withModifier (Chord ms k) m = Chord (S.insert m ms) k

key :: Key -> Chord
key = Chord S.empty
