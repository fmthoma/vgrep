module Vgrep.System.Grep
    ( grep
    , grepFiles
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import System.Environment (getArgs)
import System.Exit
import System.Process

import Vgrep.Parser

import System.IO

grep :: MonadIO io => Text -> io Text
grep input = liftIO $ do
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

grepFiles :: MonadIO io => io Text
grepFiles = liftIO $ do
    args <- getArgs
    let grepArgs = withFileName
                 : withLineNumber
                 : skipBinaryFiles
                 : lineBuffered
                 : args
    (_hIn, hOut) <- createGrepProcess grepArgs
    grepOutput <- T.hGetContents hOut
    when (T.null grepOutput) exitFailure
    pure grepOutput

withFileName, withLineNumber, skipBinaryFiles, lineBuffered :: String
withFileName    = "-H"
withLineNumber  = "-n"
skipBinaryFiles = "-I"
lineBuffered    = "--line-buffered"


createGrepProcess :: [String] -> IO (Handle, Handle)
createGrepProcess args = do
    (Just hIn, Just hOut, _hErr, _processHandle) <- createProcess
        (proc "grep" args) { std_in  = CreatePipe, std_out = CreatePipe }
    hSetBuffering hIn  LineBuffering
    hSetBuffering hOut LineBuffering
    pure (hIn, hOut)
