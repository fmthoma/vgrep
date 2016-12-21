{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Vgrep.App.Internal where

import           Control.Concurrent.Async
import           Control.Exception
import           Graphics.Vty             (Vty)
import qualified Graphics.Vty             as Vty
import           Pipes                    hiding (next)
import           System.Posix.IO
import           System.Posix.Types       (Fd)

import Vgrep.Type


-- | 'User' events do have higher priority than 'System' events, so that
-- the application stays responsive even in case of event queue congestion.
data EventPriority = User | System deriving (Eq, Ord, Enum)


-- | We need the display region in order to initialize the app, which in
-- turn will start 'Vty.Vty'. To resolve this circular dependency, we start
-- once 'Vty.Vty' in order to determine the display region, and shut it
-- down again immediately.
displayRegionHack :: IO DisplayRegion
displayRegionHack = withVty (Vty.displayBounds . Vty.outputIface)


-- | Spawns a thread parallel to the action that listens to 'Vty' events and
-- redirects them to the 'Consumer'.
withEvThread :: Consumer Vty.Event IO () -> Vty -> VgrepT s IO a -> VgrepT s IO a
withEvThread sink vty =
    vgrepBracket createEvThread cancel . const
  where
    createEvThread = (async . runEffect) $ lift (Vty.nextEvent vty) >~ sink


-- | Passes a 'Vty' instance to the action and shuts it down properly after the
-- action finishes. The 'Vty.inputFd' and 'Vty.outputFd' handles are connected
-- to @/dev/tty@ (see 'tty').
withVty :: (Vty -> IO a) -> IO a
-- | Like 'withVty', but lifted to @'VgrepT' s 'IO'@.
withVgrepVty :: (Vty -> VgrepT s IO a) -> VgrepT s IO a
(withVty, withVgrepVty) =
    let initVty fd = do
            cfg <- Vty.standardIOConfig
            Vty.mkVty cfg { Vty.inputFd  = Just fd
                          , Vty.outputFd = Just fd }
    in  ( \action -> withTty      $ \fd -> bracket      (initVty fd) Vty.shutdown action
        , \action -> withVgrepTty $ \fd -> vgrepBracket (initVty fd) Vty.shutdown action)


-- | Passes two file descriptors for read and write access to @/dev/tty@ to the
-- action. After the action has finished, the file descriptors will be closed
-- again.
withTty :: (Fd -> IO a) -> IO a
-- | Like 'withTty', but lifted to @'VgrepT' s 'IO'@.
withVgrepTty :: (Fd -> VgrepT s IO a) -> VgrepT s IO a
(withTty, withVgrepTty) = (bracket before after, vgrepBracket before after)
  where
    before = tty
    after fd = closeFd fd `catch` ignoreIOException
    ignoreIOException :: IOException -> IO ()
    ignoreIOException _ = pure ()

-- | Opens @/dev/tty@ in Read/Write mode. Should be connected to the @stdin@ and
-- @stdout@ of a GUI process (e. g. 'Vty.Vty').
tty :: IO Fd
tty = openFd "/dev/tty" ReadWrite Nothing defaultFileFlags
