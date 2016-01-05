{-# LANGUAGE RecordWildCards, LambdaCase #-}
module Main where

import Data.Foldable
import Data.Monoid
import Data.Sequence hiding (update)
import Graphics.Vty

import Vgrep.App

main :: IO ()
main = do
    ls <- fmap lines (getContents)
    let initialState = PagerState { bufferPre   = empty
                                  , currentLine = head ls
                                  , bufferPost  = fromList (tail ls) }
    _finalState <- runApp app initialState
    return ()

data PagerState = PagerState { bufferPre   :: Seq String
                             , currentLine :: String
                             , bufferPost  :: Seq String }

app :: App Event PagerState
app = App { liftEvent   = id
          , handleEvent = eventHandler
          , render      = renderer }

renderer :: Renderer PagerState
renderer PagerState{..} =
    let dpls = (fmap fore bufferPre |> back currentLine) >< fmap fore bufferPost
    in  picForImage (fold dpls)
  where
    fore = string (defAttr `withForeColor` green) . (' ':)
    back = string (defAttr `withBackColor` blue) . (' ':)

eventHandler :: EventHandler Event PagerState
eventHandler = exitOn    (KChar 'q') []
            <> handleKey KUp         [] previousLine
            <> handleKey KDown       [] nextLine
            <> handleKey (KChar 'd') [] deleteLine
            <> handleKey (KChar 'D') [] (deleteLine . previousLine)
            <> handleResize             id

handleKey :: Key -> [Modifier] -> (s -> s) -> EventHandler Event s
handleKey key modifiers action = EventHandler $ \state -> \case
    EvKey k ms | k == key && ms == modifiers -> (return . Continue . action) state
    _                                        -> return Unchanged

handleResize :: (s -> s) -> EventHandler Event s
handleResize action = EventHandler $ \state -> \case
    EvResize _ _ -> (return . Continue . action) state
    _            -> return Unchanged

exitOn :: Key -> [Modifier] -> EventHandler Event s
exitOn key modifiers = EventHandler $ \state -> \case
    EvKey k ms | k == key && ms == modifiers -> (return . Halt) state
    _                                        -> return Unchanged

nextLine :: PagerState -> PagerState
nextLine state@PagerState{..} = case viewl bufferPost of
    EmptyL  -> state
    l :< ls -> state { bufferPre   = bufferPre |> currentLine
                     , currentLine = l
                     , bufferPost  = ls }

previousLine :: PagerState -> PagerState
previousLine state@PagerState{..} = case viewr bufferPre of
    EmptyR  -> state
    ls :> l -> state { bufferPre   = ls
                     , currentLine = l
                     , bufferPost  = currentLine <| bufferPost }

deleteLine :: PagerState -> PagerState
deleteLine state@PagerState{..} = case viewl bufferPost of
    EmptyL  -> state
    l :< ls -> state { currentLine = l
                     , bufferPost  = ls }
