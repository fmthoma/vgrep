module Main where

import Data.Foldable
import Data.List
import Graphics.Vty

main :: IO ()
main = do
    cfg <- standardIOConfig
    vty <- mkVty cfg
    ls <- fmap lines (readFile "vgrep.cabal")
    eventLoop vty ls 0
    shutdown vty

eventLoop :: Vty -> [String] -> Int -> IO ()
eventLoop vty ls ln = do
    let dpls = fmap fore (take ln ls)
            ++ [ back (ls !! ln) ]
            ++ fmap fore (drop (ln + 1) ls)
        img = fold dpls
        pic = picForImage img
    update vty pic
    e <- nextEvent vty
    case e of
        EvKey KUp    [] -> eventLoop vty ls (if ln == 0 then 0 else ln - 1)
        EvKey KDown  [] -> eventLoop vty ls (if ln < length ls - 1 then ln + 1 else length ls - 1)
        EvKey _      [] -> return ()
  where
    fore = string (defAttr `withForeColor` green) . (' ':)
    back = string (defAttr `withBackColor` blue) . (' ':)
