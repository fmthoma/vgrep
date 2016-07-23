{-# LANGUAGE TemplateHaskell #-}
module Vgrep.Widget.EdLine (
      EdLineWidget
    , EdLine ()
    , edLine
    , edLineWidget
    , cursorPos
    , command
    , clear
    , insert
    , delete
    , backspace
    ) where

import           Control.Lens              hiding (pre)
import           Control.Monad.State
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.Text.Zipper          (TextZipper)
import qualified Data.Text.Zipper          as Zipper
import           Graphics.Vty.Image
import           Graphics.Vty.Input.Events
import           Vgrep.Type
import           Vgrep.Widget.Type

type EdLineWidget = Widget EdLine

newtype EdLine = EdLine { _zipper :: TextZipper Text }

makeLenses ''EdLine

cursorPos :: Getter EdLine Int
cursorPos = zipper . to Zipper.cursorPosition . _2

command :: Getter EdLine Text
command = zipper . to Zipper.getText . to head
-- We have to maintain the invariant that the TextZipper has exactly one line!

edLineWidget :: EdLineWidget
edLineWidget = Widget
    { initialize = edLine
    , draw       = drawWidget
    , cursor     = getCursor
    , handle     = handleEvent }

edLine :: EdLine
edLine = EdLine $ Zipper.mkZipper
    Text.singleton
    Text.drop
    Text.take
    Text.length
    Text.last
    Text.init
    Text.null
    [Text.empty]
    (Just 1)

clear :: Monad m => VgrepT EdLine m Redraw
clear = put edLine >> pure Redraw >> pure Redraw

insert :: Monad m => Char -> VgrepT EdLine m Redraw
insert chr = modifying zipper (Zipper.insertChar chr) >> pure Redraw

delete :: Monad m => VgrepT EdLine m Redraw
delete = modifying zipper Zipper.deleteChar >> pure Redraw

backspace :: Monad m => VgrepT EdLine m Redraw
backspace = modifying zipper Zipper.deletePrevChar >> pure Redraw

drawWidget :: Monad m => VgrepT EdLine m Image
drawWidget = do
    (width, _) <- view region
    cmdText <- use command
    pure (text' defAttr (Text.justifyLeft width ' ' cmdText))

getCursor :: EdLine -> Cursor
getCursor = views cursorPos (\pos -> Cursor pos 0)

handleEvent :: Monad m => Event -> EdLine -> Next (VgrepT EdLine m Redraw)
handleEvent event _ = Continue $ case event of
    EvKey (KChar chr) [] -> insert chr
    EvKey KBS         [] -> backspace
    EvKey KDel        [] -> delete
    _otherwise           -> pure Unchanged
