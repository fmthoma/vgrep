{-# LANGUAGE TemplateHaskell #-}
module Vgrep.Environment
    ( Environment (..)
    , Viewport (..)

    -- * Auto-generated Lenses
    , config
    , viewport
    , viewportWidth
    , viewportHeight

    -- * Re-exports
    , module Vgrep.Environment.Config
    ) where

import Control.Lens.Compat

import Vgrep.Environment.Config


-- | The bounds (width and height) of a display viewport.
data Viewport = Viewport { _viewportWidth :: Int, _viewportHeight :: Int }
    deriving (Eq, Show)

makeLenses ''Viewport

-- | 'Vgrep.Type.VgrepT' actions can read from the environment.
data Environment = Env
    { _config :: Config
    -- ^ External configuration (colors, editor executable, …)

    , _viewport :: Viewport
    -- ^ The bounds (width and height) of the display viewport where the
    -- 'Vgrep.App.App' or the current 'Vgrep.Widget.Widget' is displayed
    } deriving (Eq, Show)

makeLenses ''Environment
