{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Foldable
import Data.Sequence hiding (update)
import Graphics.Vty
import System.Posix

main :: IO ()
main = do
    ls <- fmap lines (getContents)
    cfg <- standardIOConfig
    tty <- openFd "/dev/tty" ReadOnly Nothing defaultFileFlags
    vty <- mkVty (cfg { inputFd = Just tty })
    eventLoop $ PagerState { bufferPre   = empty
                           , currentLine = head ls
                           , bufferPost  = fromList (tail ls)
                           , display     = vty }
    shutdown vty

data PagerState = PagerState { bufferPre   :: Seq String
                             , currentLine :: String
                             , bufferPost  :: Seq String
                             , display     :: Vty }

eventLoop :: PagerState -> IO ()
eventLoop state@PagerState{..} = do
    let dpls = (fmap fore bufferPre |> back currentLine) >< fmap fore bufferPost
        img = fold dpls
        pic = picForImage img
    update display pic
    e <- nextEvent display
    case e of
        EvKey KUp    [] -> eventLoop (previousLine state)
        EvKey KDown  [] -> eventLoop (nextLine state)
        EvKey _      [] -> return ()
  where
    fore = string (defAttr `withForeColor` green) . (' ':)
    back = string (defAttr `withBackColor` blue) . (' ':)

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
