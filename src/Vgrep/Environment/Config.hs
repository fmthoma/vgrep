{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Vgrep.Environment.Config where

import Control.Lens
import Control.Monad.IO.Class
import Data.Maybe
import Data.Monoid
import Graphics.Vty.Image

import Vgrep.Environment.Config.Monoid
import Vgrep.Environment.Config.Sources


--------------------------------------------------------------------------
-- * Types
--------------------------------------------------------------------------

data Config = Config
    { _colors  :: Colors
    -- ^ Color configuration

    , _tabstop :: Int
    -- ^ Tabstop width (default: 8)

    , _editor  :: String
    -- ^ Executable for @e@ key (default: environment variable @$EDITOR@,
    -- or @vi@ if @$EDITOR@ is not set)

    } deriving (Eq, Show)

data Colors = Colors
    { _lineNumbers   :: Attr
    -- ^ Line numbers (default: blue)

    , _lineNumbersHl :: Attr
    -- ^ Highlighted line numbers (default: bold blue)

    , _normal        :: Attr
    -- ^ Normal text (default: terminal default)

    , _normalHl      :: Attr
    -- ^ Highlighted text (default: bold)

    , _fileHeaders   :: Attr
    -- ^ File names in results view (default: terminal default color on
    -- green background)

    , _selected      :: Attr
    -- ^ Selected entry (default: terminal default, inverted)

    } deriving (Eq, Show)


--------------------------------------------------------------------------
-- * Auto-generated Lenses
--------------------------------------------------------------------------

makeLenses ''Config
makeLenses ''Colors


--------------------------------------------------------------------------
-- * Read Config from Monoid
--------------------------------------------------------------------------

fromConfigMonoid :: ConfigMonoid -> Config
fromConfigMonoid ConfigMonoid{..} = Config
    { _colors  = fromColorsMonoid _mcolors
    , _tabstop = fromFirst (_tabstop defaultConfig) _mtabstop
    , _editor  = fromFirst (_editor  defaultConfig) _meditor }

fromColorsMonoid :: ColorsMonoid -> Colors
fromColorsMonoid ColorsMonoid{..} = Colors
    { _lineNumbers   = fromFirst (_lineNumbers   defaultColors) _mlineNumbers
    , _lineNumbersHl = fromFirst (_lineNumbersHl defaultColors) _mlineNumbersHl
    , _normal        = fromFirst (_normal        defaultColors) _mnormal
    , _normalHl      = fromFirst (_normalHl      defaultColors) _mnormalHl
    , _fileHeaders   = fromFirst (_fileHeaders   defaultColors) _mfileHeaders
    , _selected      = fromFirst (_selected      defaultColors) _mselected }

fromFirst :: a -> First a -> a
fromFirst a = fromMaybe a . getFirst


--------------------------------------------------------------------------
-- * Default Config
--------------------------------------------------------------------------

defaultConfig :: Config
defaultConfig = Config
    { _colors = defaultColors
    , _tabstop = 8
    , _editor = "vi" }

defaultColors :: Colors
defaultColors = Colors
    { _lineNumbers   = defAttr `withForeColor` blue
    , _lineNumbersHl = defAttr `withForeColor` blue
                               `withStyle` bold
    , _normal        = defAttr
    , _normalHl      = defAttr `withStyle` bold
    , _fileHeaders   = defAttr `withBackColor` green
    , _selected      = defAttr `withStyle` standout }


--------------------------------------------------------------------------
-- * Config Loader
--------------------------------------------------------------------------

-- | Gathers 'ConfigMonoid's from various sources and builds a 'Config'
-- based on the 'defaultConfig'.
loadConfig
    :: MonadIO io
    => ConfigMonoid -- ^ External config from command line
    -> io Config
loadConfig configFromArgs = do
    configs <- sequence
        [ pure configFromArgs
        , editorConfigFromEnv
        , configFromFile ]
    pure (fromConfigMonoid (mconcat configs))
