{-# LANGUAGE RecordWildCards, LambdaCase #-}
module Main where

import Data.Foldable
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
eventHandler = EventHandler $ \state -> \case
    EvKey KUp         [] -> return (Continue (previousLine state))
    EvKey KDown       [] -> return (Continue (nextLine state))
    EvKey (KChar 'q') [] -> return (Halt state)
    EvKey (KChar 'd') [] -> return (Continue (deleteLine state))
    EvKey (KChar 'D') [] -> return (Continue ((deleteLine . previousLine) state))
    EvResize w h         -> return Unchanged
    ev                   -> do
      putStrLn (show ev)
      return Unchanged

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
