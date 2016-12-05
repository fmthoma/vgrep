module Vgrep.Environment.Config.Sources.Env where

import Control.Monad.IO.Class
import Data.Monoid
import System.Environment

import Vgrep.Environment.Config.Monoid


-- | Determines the 'ConfigMonoid' value for 'Vgrep.Environment.Config._editor'
-- ('_meditor') from the environment variable @$EDITOR@.
editorConfigFromEnv :: MonadIO io => io ConfigMonoid
editorConfigFromEnv = do
    configuredEditor <- liftIO (lookupEnv "EDITOR")
    pure (mempty { _meditor = First configuredEditor })
