{-# LANGUAGE RecordWildCards, MultiWayIf #-}
module Vgrep.Widget.List ( ListState()
                         , ListWidget
                         , listWidget
                         ) where

import Data.Foldable
import Data.Monoid
import Data.Sequence hiding (update, take, empty)
import qualified Data.Sequence as Seq
import Graphics.Vty
import Graphics.Vty.Prelude

import Vgrep.Event
import Vgrep.Widget.Type

data ListState = ListState { _bufferPre   :: Seq String
                           , _currentLine :: String
                           , _bufferPost  :: Seq String
                           , _scrollPos   :: Int
                           , _region      :: DisplayRegion }

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
    hd:tl -> ListState { bufferPre   = Seq.empty
                       , currentLine = hd
                       , bufferPost  = fromList tl
                       , scrollPos   = 0
                       , region      = region }

handleListEvents :: EventHandler ListState
handleListEvents = handleKey KUp         [] (updateScrollPos . previousLine)
                <> handleKey KDown       [] (updateScrollPos . nextLine)

renderList :: ListState -> Image
renderList ListState{..} =
    let dpls = (fmap fore bufferPre |> back currentLine) >< fmap fore bufferPost
    in  (fold . Seq.drop scrollPos) dpls
  where
    fore = string (defAttr `withForeColor` green) . truncate
    back = string (defAttr `withBackColor` blue) . truncate
    truncate s = let width = regionWidth region
                 in  take width (' ' : s ++ repeat ' ') ++ " "

nextLine :: ListState -> ListState
nextLine state@ListState{..} = case viewl bufferPost of
    EmptyL  -> state
    l :< ls -> state { bufferPre   = bufferPre |> currentLine
                     , currentLine = l
                     , bufferPost  = ls }

previousLine :: ListState -> ListState
previousLine state@ListState{..} = case viewr bufferPre of
    EmptyR  -> state
    ls :> l -> state { bufferPre   = ls
                     , currentLine = l
                     , bufferPost  = currentLine <| bufferPost }

deleteLine :: ListState -> ListState
deleteLine state@ListState{..} = case viewl bufferPost of
    EmptyL  -> state
    l :< ls -> state { currentLine = l
                     , bufferPost  = ls }

resizeToRegion :: DisplayRegion -> ListState -> ListState
resizeToRegion newRegion state = updateScrollPos $ state { region = newRegion }

updateScrollPos :: ListState -> ListState
updateScrollPos state@ListState{..} =
    if | current < firstVisible -> state { scrollPos = current }
       | current > lastVisible  -> state { scrollPos = current - height + 1 }
       | otherwise              -> state
  where
    height       = regionHeight region
    current      = Seq.length bufferPre
    firstVisible = scrollPos
    lastVisible  = scrollPos + height - 1
