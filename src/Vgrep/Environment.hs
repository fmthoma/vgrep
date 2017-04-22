{-# LANGUAGE TemplateHaskell #-}
module Vgrep.Environment
    ( Environment (..)
    , Viewport (..)

    -- * Auto-generated Lenses
    , config
    , viewport
    , vpHeight
    , vpWidth
    , searchRegex

    -- * Convenience Lenses
    , viewportWidth
    , viewportHeight

    -- * Re-exports
    , module Vgrep.Environment.Config
    ) where

import Control.Lens.Compat
import Text.Regex.TDFA.Text
import Data.Text

import Vgrep.Environment.Config


-- | The bounds (width and height) of a display viewport.
data Viewport = Viewport { _vpWidth :: Int, _vpHeight :: Int }
    deriving (Eq, Show)

makeLenses ''Viewport


-- | 'Vgrep.Type.VgrepT' actions can read from the environment.
data Environment = Env
    { _config      :: Config
    -- ^ External configuration (colors, editor executable, …)

    , _viewport    :: Viewport
    -- ^ The bounds (width and height) of the display viewport where the
    -- 'Vgrep.App.App' or the current 'Vgrep.Widget.Widget' is displayed

    , _searchRegex :: Maybe (Regex, Text)
    -- ^ The currently active search regex. The 'Text' parameter is for printing
    -- the regex, since 'Regex' does not have a 'Show' instance.
    }

instance Show Environment where
    show env = mconcat
        [ "Env {"
        , "_config = ", show (_config env) , ","
        , "_viewport = ", show (_viewport env), ","
        , "_searchRegex = ", show (fmap snd (_searchRegex env)), ","
        , "}" ]

makeLenses ''Environment


viewportHeight, viewportWidth :: Lens' Environment Int
viewportHeight = viewport . vpHeight
viewportWidth  = viewport . vpWidth
