{-# LANGUAGE Rank2Types #-}
module Vgrep.System.Grep
    ( grep
    , grepForApp
    , recursiveGrep
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Data.Maybe
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Pipes as P
import qualified Pipes.Prelude as P
import System.Environment (getArgs)
import System.Exit
import System.Process

import Vgrep.Parser
import Vgrep.Type

import System.IO

grepForApp :: Producer Text IO () -> Producer Text IO ()
grepForApp input = do
    (firstInputLine, input') <- peek input
    when (isNothing firstInputLine) (lift exitFailure)
    case firstInputLine >>= parseLine of
        Just _line -> grep input'
        Nothing    -> grepWithFileAndLineNumber input'

grepWithFileAndLineNumber :: Producer Text IO () -> Producer Text IO ()
grepWithFileAndLineNumber input = do
    args <- liftIO getArgs
    grepPipe (withFileName : withLineNumber : args) input

grep :: Producer Text IO () -> Producer Text IO ()
grep input = do
    args <- liftIO getArgs
    grepPipe args input

grepPipe :: [String] -> Producer Text IO () -> Producer Text IO ()
grepPipe args input = do
    (hIn, hOut) <- lift (createGrepProcess (lineBuffered : args))
    _threadId <- liftIO . forkIO . runEffect $ input >-> textToHandle hIn
    (maybeFirstLine, grepOutput) <- peek (textFromHandle hOut)
    when (isNothing maybeFirstLine) (lift exitFailure)
    grepOutput

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

textFromHandle :: MonadIO m => Handle -> Producer' Text m ()
textFromHandle h = P.fromHandle h >-> P.map T.pack

textToHandle :: MonadIO m => Handle -> Consumer' Text m ()
textToHandle h = P.map T.unpack >-> P.toHandle h

peek :: Monad m => Producer a m r -> Producer a m (Maybe a, Producer a m r)
peek producer = do
    eitherNext <- lift (next producer)
    pure $ case eitherNext of
        Left r               -> (Nothing, pure r)
        Right (a, producer') -> (Just a,  P.yield a >> producer')
