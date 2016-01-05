{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Foldable
import Data.List
import Graphics.Vty
import System.Posix

main :: IO ()
main = do
    ls <- fmap lines (getContents)
    cfg <- standardIOConfig
    tty <- openFd "/dev/tty" ReadOnly Nothing defaultFileFlags
    vty <- mkVty (cfg { inputFd = Just tty })
    eventLoop (PagerState ls 0 vty)
    shutdown vty

data PagerState = PagerState { lineBuffer  :: [String]
                             , currentLine :: Int
                             , display     :: Vty }

eventLoop :: PagerState -> IO ()
eventLoop state@PagerState{..} = do
    let dpls = fmap fore (take currentLine lineBuffer)
            ++ [ back (lineBuffer !! currentLine) ]
            ++ fmap fore (drop (currentLine + 1) lineBuffer)
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
nextLine state = let line = currentLine state + 1
                 in  if line < length (lineBuffer state)
                         then state { currentLine = line }
                         else state { currentLine = length (lineBuffer state) - 1 }

previousLine :: PagerState -> PagerState
previousLine state = let line = currentLine state - 1
                     in  if line >= 0
                             then state { currentLine = line }
                             else state { currentLine = 0 }
