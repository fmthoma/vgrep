module Vgrep.System.Grep
    ( grepStdin
    , grepStdinForApp
    , recursiveGrep
    ) where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import System.Environment (getArgs)
import System.Exit
import System.Process

import Vgrep.Type
import qualified Vgrep.Environment as Env
import Vgrep.Parser

import System.IO

grepStdinForApp :: VgrepT IO Text
grepStdinForApp = do
    firstInputLine <- preview (Env.input . to T.lines . traverse)
    case firstInputLine >>= parseLine of
        Just _line -> grepStdin
        Nothing    -> grepStdinWithFileAndLineNumber

grepStdinWithFileAndLineNumber :: VgrepT IO Text
grepStdinWithFileAndLineNumber = do
    input <- view Env.input
    args <- liftIO getArgs
    liftIO (grepText (withFileName : withLineNumber : args) input)

grepStdin :: VgrepT IO Text
grepStdin = do
    input <- view Env.input
    args <- liftIO getArgs
    liftIO (grepText args input)

grepText :: [String] -> Text -> IO Text
grepText args input = do
    when (T.null input) exitFailure
    (hIn, hOut) <- createGrepProcess (lineBuffered : args)
    _threadId <- forkIO (T.hPutStr hIn input)
    grepOutput <- T.hGetContents hOut
    when (T.null grepOutput) exitFailure
    pure grepOutput

recursiveGrep :: MonadIO io => io Text
recursiveGrep = liftIO $ do
    args <- getArgs
    let grepArgs = recursive
                 : withFileName
                 : withLineNumber
                 : skipBinaryFiles
                 : lineBuffered
                 : args
    (_hIn, hOut) <- createGrepProcess grepArgs
    grepOutput <- T.hGetContents hOut
    when (T.null grepOutput) exitFailure
    pure grepOutput

recursive, withFileName, withLineNumber, skipBinaryFiles, lineBuffered :: String
recursive       = "-r"
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
