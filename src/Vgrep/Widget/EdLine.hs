{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
module Vgrep.Widget.EdLine (
      EdLineWidget
    , EdLine ()
    , edLine
    , edLineWidget
    , cursorPos
    , edLineText
    , reset
    , clear

    , putStatus
    , enterSearch

    , insert
    , delete
    , backspace
    , deleteWord
    , deletePrevWord
    , moveLeft
    , moveRight
    , moveWordLeft
    , moveWordRight
    , moveHome
    , moveEnd
    ) where

import           Control.Lens                   hiding (pre)
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import           Data.Text.Zipper               (TextZipper)
import qualified Data.Text.Zipper               as Zipper
import qualified Data.Text.Zipper.Generic.Words as Zipper
import           Graphics.Vty.Attributes
import           Graphics.Vty.Image
import           Vgrep.Type
import           Vgrep.Widget.Type

type EdLineWidget = Widget EdLine

data EdLine = EdLine
    { _mode   :: Mode
    , _zipper :: TextZipper Text }

data Mode = Search | Status

makeLenses ''EdLine

cursorPos :: Getter EdLine Int
cursorPos = zipper . to Zipper.cursorPosition . _2

edLineText :: Getter EdLine Text
edLineText = zipper . to Zipper.getText . to head
-- We have to maintain the invariant that the TextZipper has exactly one line!

edLineWidget :: EdLineWidget
edLineWidget = Widget
    { initialize = edLine
    , draw       = drawWidget
    , cursor     = getCursor }

edLine :: EdLine
edLine = EdLine
    { _mode = Status
    , _zipper = emptyZipper }


emptyZipper :: TextZipper Text
emptyZipper = zipperOf Text.empty

zipperOf :: Text -> TextZipper Text
zipperOf txt = Zipper.textZipper [txt] (Just 1)

reset :: Monad m => VgrepT EdLine m Redraw
reset = put edLine >> pure Redraw

clear :: Monad m => VgrepT EdLine m Redraw
clear = assign zipper emptyZipper >> pure Redraw


putStatus :: Monad m => Text -> VgrepT EdLine m Redraw
putStatus txt = do
    put EdLine { _mode = Status, _zipper = zipperOf txt }
    pure Redraw

enterSearch :: Monad m => VgrepT EdLine m Redraw
enterSearch = do
    put EdLine { _mode = Search, _zipper = emptyZipper }
    pure Redraw


insert :: Monad m => Char -> VgrepT EdLine m Redraw
insert chr = edit (Zipper.insertChar chr)

delete :: Monad m => VgrepT EdLine m Redraw
delete = edit Zipper.deleteChar

backspace :: Monad m => VgrepT EdLine m Redraw
backspace = edit Zipper.deletePrevChar

deleteWord :: Monad m => VgrepT EdLine m Redraw
deleteWord = edit Zipper.deleteWord

deletePrevWord :: Monad m => VgrepT EdLine m Redraw
deletePrevWord = edit Zipper.deletePrevWord

moveLeft :: Monad m => VgrepT EdLine m Redraw
moveLeft = edit Zipper.moveLeft

moveRight :: Monad m => VgrepT EdLine m Redraw
moveRight = edit Zipper.moveRight

moveHome :: Monad m => VgrepT EdLine m Redraw
moveHome = edit Zipper.gotoBOL

moveEnd :: Monad m => VgrepT EdLine m Redraw
moveEnd = edit Zipper.gotoEOL

moveWordLeft :: Monad m => VgrepT EdLine m Redraw
moveWordLeft = edit Zipper.moveWordLeft

moveWordRight :: Monad m => VgrepT EdLine m Redraw
moveWordRight = edit Zipper.moveWordRight

edit :: Monad m => (TextZipper Text -> TextZipper Text) -> VgrepT EdLine m Redraw
edit action = use mode >>= \case
    Status -> pure Unchanged
    Search -> modifying zipper action >> pure Redraw

drawWidget :: Monad m => VgrepT EdLine m Image
drawWidget = use mode >>= \case
    Status -> use edLineText >>= render
    Search -> use edLineText <&> Text.cons '/' >>= render
  where
    render txt = do
        width <- view viewportWidth
        pure (text' defAttr (Text.justifyLeft width ' ' txt))

getCursor :: Monad m => VgrepT EdLine m Cursor
getCursor = use mode >>= \case
    Status -> pure NoCursor
    _otherwise -> uses cursorPos (\pos -> Cursor (pos + 1) 0)
