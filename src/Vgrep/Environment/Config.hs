{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Vgrep.Environment.Config where

import           Control.Lens.Compat
import           Control.Monad.IO.Class
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as M
import           Data.Maybe
import           Data.Monoid
import           Graphics.Vty.Image
    ( Attr
    , blue
    , bold
    , defAttr
    , green
    , standout
    , withBackColor
    , withForeColor
    , withStyle
    )
import           Graphics.Vty.Input.Events (Key (..), Modifier (..))

import Vgrep.Commands
import Vgrep.Environment.Config.Monoid
import Vgrep.Environment.Config.Sources


--------------------------------------------------------------------------
-- * Types
--------------------------------------------------------------------------

data Config = Config
    { _colors      :: Colors
    -- ^ Color configuration

    , _tabstop     :: Int
    -- ^ Tabstop width (default: 8)

    , _editor      :: String
    -- ^ Executable for @e@ key (default: environment variable @$EDITOR@,
    -- or @vi@ if @$EDITOR@ is not set)

    , _keybindings :: Keybindings

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

data Keybindings = Keybindings
    { _resultsKeybindings :: Map (Key, [Modifier]) Command
    , _pagerKeybindings   :: Map (Key, [Modifier]) Command
    , _globalKeybindings  :: Map (Key, [Modifier]) Command
    } deriving (Eq, Show)


--------------------------------------------------------------------------
-- * Auto-generated Lenses
--------------------------------------------------------------------------

makeLenses ''Config
makeLenses ''Colors
makeLenses ''Keybindings


--------------------------------------------------------------------------
-- * Read Config from Monoid
--------------------------------------------------------------------------

-- | Convert a 'ConfigMonoid' to a 'Config'. Missing (@'mempty'@) values in the
-- 'ConfigMonoid' are supplied from the 'defaultConfig'.
fromConfigMonoid :: ConfigMonoid -> Config
fromConfigMonoid ConfigMonoid{..} = Config
    { _colors  = fromColorsMonoid _mcolors
    , _tabstop = fromFirst (_tabstop defaultConfig) _mtabstop
    , _editor  = fromFirst (_editor  defaultConfig) _meditor
    , _keybindings = fromKeybindingsMonoid _mkeybindings }

-- | Convert a 'ColorsMonoid' to a 'Colors' config.
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

fromKeybindingsMonoid :: KeybindingsMonoid -> Keybindings
fromKeybindingsMonoid KeybindingsMonoid{..} = Keybindings
    { _resultsKeybindings = _mresultsKeybindings <> _resultsKeybindings defaultKeybindings
    , _pagerKeybindings   = _mpagerKeybindings   <> _pagerKeybindings   defaultKeybindings
    , _globalKeybindings  = _mglobalKeybindings  <> _globalKeybindings  defaultKeybindings }


--------------------------------------------------------------------------
-- * Default Config
--------------------------------------------------------------------------

defaultConfig :: Config
defaultConfig = Config
    { _colors = defaultColors
    , _tabstop = 8
    , _editor = "vi"
    , _keybindings = defaultKeybindings }

defaultColors :: Colors
defaultColors = Colors
    { _lineNumbers   = defAttr `withForeColor` blue
    , _lineNumbersHl = defAttr `withForeColor` blue
                               `withStyle` bold
    , _normal        = defAttr
    , _normalHl      = defAttr `withStyle` bold
    , _fileHeaders   = defAttr `withBackColor` green
    , _selected      = defAttr `withStyle` standout }

defaultKeybindings :: Keybindings
defaultKeybindings = Keybindings
    { _resultsKeybindings = M.fromList
        [ ((KUp,        []), ResultsUp)
        , ((KDown,      []), ResultsDown)
        , ((KPageUp,    []), ResultsPgUp)
        , ((KPageDown,  []), ResultsPgDown)
        , ((KEnter,     []), PagerGotoResult)
        , ((KChar 'f',  []), DisplayResultsOnly)
        , ((KChar '\t', []), SplitFocusPager)
        , ((KChar 'q',  []), Exit) ]
    , _pagerKeybindings = M.fromList
        [ ((KUp,        []), PagerUp)
        , ((KDown,      []), PagerDown)
        , ((KPageUp,    []), PagerPgUp)
        , ((KPageDown,  []), PagerPgDown)
        , ((KLeft,      []), PagerScrollLeft)
        , ((KRight,     []), PagerScrollRight)
        , ((KChar 'f',  []), DisplayPagerOnly)
        , ((KChar '\t', []), SplitFocusResults)
        , ((KChar 'q',  []), DisplayResultsOnly) ]
    , _globalKeybindings = M.fromList
        [ ((KChar 'e',  []), OpenFileInEditor) ]
    }


--------------------------------------------------------------------------
-- * Config Loader
--------------------------------------------------------------------------

-- | Gathers 'ConfigMonoid's from various sources and builds a 'Config'
-- based on the 'defaultConfig':
--
-- * Config from environment variables
-- * The configuration specified in the config file
-- * External config, e.g. from command line
--
-- where the latter ones override the earlier ones.
loadConfig
    :: MonadIO io
    => ConfigMonoid -- ^ External config from command line
    -> io Config
loadConfig configFromArgs = do
    configs <- sequence
        [ pure configFromArgs
        , configFromFile
        , editorConfigFromEnv ]
    pure (fromConfigMonoid (mconcat configs))
