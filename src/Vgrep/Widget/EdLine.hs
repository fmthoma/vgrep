{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
module Vgrep.Widget.EdLine (
      EdLineWidget
    , EdLine ()
    , edLineWidget
    , searchText
    , reset
    , clear

    , _Search
    , _Status

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

import           Control.Lens
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

data EdLine
    = Status Text
    | Search (TextZipper Text)

makePrisms ''EdLine

cursorPos :: Fold EdLine Int
cursorPos = _Search . to Zipper.cursorPosition . _2

searchText :: Fold EdLine Text
searchText = _Search . to Zipper.getText . to head
-- We have to maintain the invariant that the TextZipper has exactly one line!

edLineWidget :: EdLineWidget
edLineWidget = Widget
    { initialize = Status mempty
    , draw       = drawWidget
    , cursor     = getCursor }


emptyZipper :: TextZipper Text
emptyZipper = zipperOf Text.empty

zipperOf :: Text -> TextZipper Text
zipperOf txt = Zipper.textZipper [txt] (Just 1)

reset :: Monad m => VgrepT EdLine m Redraw
reset = put (Status mempty) >> pure Redraw

clear :: Monad m => VgrepT EdLine m Redraw
clear = assign _Search emptyZipper >> pure Redraw


putStatus :: Monad m => Text -> VgrepT EdLine m ()
putStatus txt = assign _Status txt

enterSearch :: Monad m => VgrepT EdLine m ()
enterSearch = put (Search emptyZipper)


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
edit action = modifying _Search action >> pure Redraw

drawWidget :: Monad m => VgrepT EdLine m Image
drawWidget = get >>= \case
    Status msg -> render msg
    Search txt -> render (Text.cons '/' (head (Zipper.getText txt)))
  where
    render txt = do
        width <- view viewportWidth
        pure (text' defAttr (Text.justifyLeft width ' ' txt))

getCursor :: Monad m => VgrepT EdLine m Cursor
getCursor = preuse cursorPos <&> maybe NoCursor (\pos -> Cursor (pos + 1) 0)
