{-# LANGUAGE RecordWildCards, MultiWayIf, TemplateHaskell #-}
module Vgrep.Widget.List ( ListState()
                         , ListWidget
                         , listWidget
                         ) where

import Control.Lens hiding ((|>), (<|), (:>), (:<))
import Control.Lens.TH
import Data.Foldable
import Data.Monoid
import Data.Sequence hiding (update, take, empty)
import qualified Data.Sequence as Seq
import Data.Sequence.Lens
import Graphics.Vty
import Graphics.Vty.Prelude

import Vgrep.Event
import Vgrep.Widget.Type

data ListState = ListState { _bufferPre   :: Seq String
                           , _currentLine :: String
                           , _bufferPost  :: Seq String
                           , _scrollPos   :: Int
                           , _region      :: DisplayRegion }

makeLenses ''ListState

type ListWidget = Widget ListState

listWidget :: [String]
           -> DisplayRegion
           -> ListWidget
listWidget items region = Widget { _state       = initialListState items region
                                 , _dimensions  = region
                                 , _resize      = resizeToRegion
                                 , _draw        = renderList
                                 , _handleEvent = handleListEvents }


initialListState :: [String] -> DisplayRegion -> ListState
initialListState items region = case items of
    []    -> initialListState [""] region
    hd:tl -> ListState { _bufferPre   = Seq.empty
                       , _currentLine = hd
                       , _bufferPost  = fromList tl
                       , _scrollPos   = 0
                       , _region      = region }

handleListEvents :: EventHandler ListState
handleListEvents = handleKey KUp         [] (updateScrollPos . previousLine)
                <> handleKey KDown       [] (updateScrollPos . nextLine)

renderList :: ListState -> Image
renderList state =
    let dpls = (fmap fore (view bufferPre state)
             |> back (view currentLine state))
             >< fmap fore (view bufferPost state)
    in  (fold . Seq.drop (view scrollPos state)) dpls
  where
    fore = string (defAttr `withForeColor` green) . truncate
    back = string (defAttr `withBackColor` blue) . truncate
    truncate s = let width = regionWidth (view region state)
                 in  take width (' ' : s ++ repeat ' ') ++ " "

nextLine :: ListState -> ListState
nextLine state = case view (bufferPost . viewL) state of
    EmptyL  -> state
    l :< ls -> state & set bufferPre ( view bufferPre state
                                    |> view currentLine state )
                     . set currentLine l
                     . set bufferPost  ls

previousLine :: ListState -> ListState
previousLine state = case view (bufferPre . viewR) state of
    EmptyR  -> state
    ls :> l -> state & set bufferPre   ls
                     . set currentLine l
                     . set bufferPost  ( view currentLine state
                                      <| view bufferPost state )

deleteLine :: ListState -> ListState
deleteLine state = case view (bufferPost . viewL) state of
    EmptyL  -> state
    l :< ls -> state & set currentLine l
                     . set bufferPost  ls

resizeToRegion :: DisplayRegion -> ListState -> ListState
resizeToRegion newRegion = updateScrollPos . set region newRegion

updateScrollPos :: ListState -> ListState
updateScrollPos state =
    if | current < firstVisible -> state & set scrollPos current
       | current > lastVisible  -> state & set scrollPos (current - height + 1)
       | otherwise              -> state
  where
    height       = regionHeight (view region state)
    current      = Seq.length (view bufferPre state)
    firstVisible = view scrollPos state
    lastVisible  = firstVisible + height - 1
