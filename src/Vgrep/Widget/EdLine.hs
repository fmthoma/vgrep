{-# LANGUAGE TemplateHaskell #-}
module Vgrep.Widget.EdLine (
      EdLineWidget
    , EdLine
    , edLine
    , edLineWidget
    , cursorPos
    , command
    , clear
    , insert
    , delete
    , backspace
    ) where

import           Control.Lens            hiding (pre)
import           Control.Monad.State
import           Data.Monoid
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Graphics.Vty.Attributes
import           Graphics.Vty.Image
import           Vgrep.Type
import           Vgrep.Widget.Type

type EdLineWidget = Widget EdLine

data EdLine = EdLine
    { _pre  :: Text
    , _post :: Text }

makeLenses ''EdLine

cursorPos :: Getter EdLine Int
cursorPos = pre . to Text.length

command :: Getter EdLine Text
command = to $ do
    prefix <- view pre
    postfix <- view post
    pure (prefix <> postfix)

edLineWidget :: EdLineWidget
edLineWidget = Widget
    { initialize = edLine
    , draw       = drawWidget
    , cursor     = getCursor }

edLine :: EdLine
edLine = EdLine Text.empty Text.empty

clear :: Monad m => VgrepT EdLine m Redraw
clear = put edLine >> pure Redraw

insert :: Monad m => Char -> VgrepT EdLine m Redraw
insert chr = modifying pre (`Text.snoc` chr) >> pure Redraw

delete :: Monad m => VgrepT EdLine m Redraw
delete = use (post . to Text.length) >>= \case
    0 -> pure Unchanged
    _ -> modifying post Text.tail >> pure Redraw

backspace :: Monad m => VgrepT EdLine m Redraw
backspace = use cursorPos >>= \case
    0 -> pure Unchanged
    _ -> modifying pre Text.init >> pure Redraw

drawWidget :: Monad m => VgrepT EdLine m Image
drawWidget = do
    width <- view viewportWidth
    cmdText <- use command
    pure (text' defAttr (Text.justifyLeft width ' ' cmdText))

getCursor :: EdLine -> Cursor
getCursor = views cursorPos (\pos -> Cursor pos 0)
