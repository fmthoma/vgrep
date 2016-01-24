module Vgrep.System.Grep
    ( grepStdin
    , grepFiles
    ) where

import Control.Monad
import Control.Concurrent
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import System.Environment (getArgs)
import System.Exit
import System.Process

import Vgrep.Parser

import System.IO

grepStdin :: IO Text
grepStdin = do
    input <- T.getContents
    when (T.null input) exitFailure
    args <- getArgs
    let firstInputLine = head (T.lines input)
        grepArgs = case parseLine firstInputLine of
            Just _parsedLine -> lineBuffered
                              : args
            Nothing          -> withFileName
                              : withLineNumber
                              : lineBuffered
                              : args
    (hIn, hOut) <- createGrepProcess grepArgs
    _threadId <- forkIO (T.hPutStr hIn input)
    grepOutput <- T.hGetContents hOut
    when (T.null grepOutput) exitFailure
    pure grepOutput

grepFiles :: IO Text
grepFiles = do
    args <- getArgs
    grep ( withFileName
         : withLineNumber
         : skipBinaryFiles
         : lineBuffered
         : args )

withFileName, withLineNumber, skipBinaryFiles, lineBuffered :: String
withFileName    = "-H"
withLineNumber  = "-n"
skipBinaryFiles = "-I"
lineBuffered    = "--line-buffered"

grep :: [String] -> IO Text
grep args = do
    (_in, out) <- createGrepProcess args
    grepOutput <- T.hGetContents out
    when (T.null grepOutput) exitFailure
    pure grepOutput

createGrepProcess :: [String] -> IO (Handle, Handle)
createGrepProcess args = do
    (Just hIn, Just hOut, _hErr, _processHandle) <- createProcess
        (proc "grep" args) { std_in  = CreatePipe, std_out = CreatePipe }
    hSetBuffering hIn  LineBuffering
    hSetBuffering hOut LineBuffering
    pure (hIn, hOut)
