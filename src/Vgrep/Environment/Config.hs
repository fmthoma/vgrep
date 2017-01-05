{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Vgrep.Environment.Config where

import           Control.Lens.Compat
import           Control.Monad.IO.Class
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as M
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

import           Vgrep.Command
import           Vgrep.Environment.Config.Monoid
import           Vgrep.Environment.Config.Sources
import qualified Vgrep.Key                        as Key


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
    { _resultsKeybindings :: Map Key.Chord Command
    , _pagerKeybindings   :: Map Key.Chord Command
    , _globalKeybindings  :: Map Key.Chord Command
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
        [ (Key.key Key.Up,          ResultsUp)
        , (Key.key Key.Down,        ResultsDown)
        , (Key.key Key.PageUp,      ResultsPgUp)
        , (Key.key Key.PageDown,    ResultsPgDown)
        , (Key.key Key.Enter,       PagerGotoResult)
        , (Key.key (Key.Char 'f'),  DisplayResultsOnly)
        , (Key.key (Key.Char '\t'), SplitFocusPager)
        , (Key.key (Key.Char 'q'),  Exit) ]
    , _pagerKeybindings = M.fromList
        [ (Key.key Key.Up,          PagerUp)
        , (Key.key Key.Down,        PagerDown)
        , (Key.key Key.PageUp,      PagerPgUp)
        , (Key.key Key.PageDown,    PagerPgDown)
        , (Key.key Key.Left,        PagerScrollLeft)
        , (Key.key Key.Right,       PagerScrollRight)
        , (Key.key (Key.Char 'f'),  DisplayPagerOnly)
        , (Key.key (Key.Char '\t'), SplitFocusResults)
        , (Key.key (Key.Char 'q'),  DisplayResultsOnly) ]
    , _globalKeybindings = M.fromList
        [ (Key.key (Key.Char 'e'),  OpenFileInEditor) ]
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
