module Vgrep.System.Grep
    ( grepStdin
    , grepFiles
    ) where

import Control.Monad
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import System.Environment (getArgs)
import System.Exit
import System.Process


grepStdin :: IO Text
grepStdin = grep Inherit =<< getArgs

grepFiles :: IO Text
grepFiles = getArgs >>= \args -> grep CreatePipe
    ( "-n" -- print line numbers
    : "-H" -- always print file name
    : "-I" -- skip binary files
    : args )

grep :: StdStream -> [String] -> IO Text
grep inputStream args = do
    (_, Just out, _, _) <- createProcess $
        (proc "grep" args) { std_in  = inputStream , std_out = CreatePipe }
    grepOutput <- T.hGetContents out
    when (T.null grepOutput) exitFailure
    pure grepOutput
