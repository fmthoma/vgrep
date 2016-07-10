{-# LANGUAGE TemplateHaskell #-}
module Vgrep.Widget.EdLine (
      EdLineWidget
    , EdLine
    , edLine
    , edLineWidget
    , clear
    , insert
    , delete
    , backspace
    ) where

import           Control.Lens
import           Control.Monad.State
import           Data.Monoid
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Graphics.Vty.Image
import           Graphics.Vty.Input.Events
import           Vgrep.Type
import           Vgrep.Widget.Type

type EdLineWidget = Widget EdLine

data EdLine = EdLine
    { _cursorPos :: Int
    , _command   :: Text }

makeLenses ''EdLine

edLineWidget :: EdLineWidget
edLineWidget = Widget
    { initialize = edLine
    , draw       = drawWidget
    , handle     = handleEvent }

edLine :: EdLine
edLine = EdLine 0 Text.empty

clear :: Monad m => VgrepT EdLine m Redraw
clear = put edLine >> pure Redraw

insert :: Monad m => Char -> VgrepT EdLine m Redraw
insert chr = do
    cmdText <- use command
    pos <- use cursorPos
    let (prefix, postfix) = Text.splitAt pos cmdText
    assign command (prefix <> Text.singleton chr <> postfix)
    assign cursorPos (pos + 1)
    pure Redraw

delete :: Monad m => VgrepT EdLine m Redraw
delete = do
    --modifying command _
    pure Redraw

backspace :: Monad m => VgrepT EdLine m Redraw
backspace = do
    modifying cursorPos (+ (-1))
    --modifying command _
    pure Redraw

drawWidget :: Monad m => VgrepT EdLine m Image
drawWidget = do
    (width, _) <- view region
    cmdText <- use command
    pure (text' defAttr (Text.justifyLeft width ' ' cmdText))

handleEvent :: Monad m => Event -> EdLine -> Next (VgrepT EdLine m Redraw)
handleEvent event _ = Continue $ case event of
    EvKey (KChar chr) [] -> insert chr
    EvKey KBS         [] -> backspace
    EvKey KDel        [] -> delete
    _otherwise           -> pure Unchanged
