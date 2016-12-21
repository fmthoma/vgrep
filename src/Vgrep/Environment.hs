{-# LANGUAGE TemplateHaskell #-}
module Vgrep.Environment
    ( Environment (..)

    -- * Auto-generated Lenses
    , config
    , region

    -- * Re-exports
    , module Vgrep.Environment.Config
    , module Graphics.Vty.Prelude
    ) where

import Control.Lens.Compat
import Graphics.Vty.Prelude

import Vgrep.Environment.Config


-- | 'Vgrep.Type.VgrepT' actions can read from the environment.
data Environment = Env
    { _config :: Config
    -- ^ External configuration (colors, editor executable, …)

    , _region :: DisplayRegion
    -- ^ The bounds (width and height) of the display region where the
    -- 'Vgrep.App.App' or the current 'Vgrep.Widget.Widget' is displayed
    } deriving (Eq, Show)

makeLenses ''Environment
