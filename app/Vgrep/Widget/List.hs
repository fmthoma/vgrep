{-# LANGUAGE RecordWildCards, MultiWayIf #-}
module Vgrep.Widget.List where

import Data.Foldable
import Data.Monoid
import Data.Sequence hiding (update, take, empty)
import qualified Data.Sequence as Seq
import Graphics.Vty
import Graphics.Vty.Prelude

import Vgrep.Event

data ListState = ListState { bufferPre   :: Seq String
                           , currentLine :: String
                           , bufferPost  :: Seq String
                           , scrollPos   :: Int
                           , region      :: DisplayRegion }


initialListState :: [String] -> DisplayRegion -> ListState
initialListState items region = case items of
    []    -> initialListState [""] region
    hd:tl -> ListState { bufferPre   = Seq.empty
                       , currentLine = hd
                       , bufferPost  = fromList tl
                       , scrollPos   = 0
                       , region      = region }

handleListEvents :: EventHandler Event ListState
handleListEvents = handleKey KUp         [] (updateScrollPos . previousLine)
                <> handleKey KDown       [] (updateScrollPos . nextLine)

renderList :: ListState -> Picture
renderList ListState{..} =
    let dpls = (fmap fore bufferPre |> back currentLine) >< fmap fore bufferPost
    in  (picForImage . fold . Seq.drop scrollPos) dpls
  where
    fore = string (defAttr `withForeColor` green) . truncate
    back = string (defAttr `withBackColor` blue) . truncate
    truncate s = let width = (regionWidth region `div` 2) - 1
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
