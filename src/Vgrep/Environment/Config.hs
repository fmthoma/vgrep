{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Vgrep.Environment.Config where

import           Control.Lens.Compat
import           Control.Monad.IO.Class
import           Data.Maybe
import           Data.Monoid
import           Graphics.Vty.Attributes
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
import           Vgrep.KeybindingMap              (KeybindingMap (..))
import qualified Vgrep.KeybindingMap              as KeybindingMap


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
    { _resultsKeybindings :: KeybindingMap
    -- ^ Keybindings in effect when results list is focused.

    , _pagerKeybindings   :: KeybindingMap
    -- ^ Keybindings in effect when pager is focused.

    , _globalKeybindings  :: KeybindingMap
    -- ^ Global keybindings are in effect both for pager and results list, but
    -- can be overridden by either one.

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
    { _resultsKeybindings = fromMaybe mempty _mresultsKeybindings <> _resultsKeybindings defaultKeybindings
    , _pagerKeybindings   = fromMaybe mempty _mpagerKeybindings   <> _pagerKeybindings   defaultKeybindings
    , _globalKeybindings  = fromMaybe mempty _mglobalKeybindings  <> _globalKeybindings  defaultKeybindings }


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
    { _resultsKeybindings = KeybindingMap.fromList
        [ (Key.key Key.Up,          ResultsUp)
        , (Key.key Key.Down,        ResultsDown)
        , (Key.key Key.PageUp,      ResultsPageUp)
        , (Key.key Key.PageDown,    ResultsPageDown)
        , (Key.key Key.Enter,       PagerGotoResult)
        , (Key.key (Key.Char 'k'),  ResultsUp)
        , (Key.key (Key.Char 'j'),  ResultsDown)
        , (Key.key (Key.Char 'u') `Key.withModifier` Key.Ctrl, ResultsPageUp)
        , (Key.key (Key.Char 'd') `Key.withModifier` Key.Ctrl, ResultsPageDown)
        , (Key.key (Key.Char 'b') `Key.withModifier` Key.Ctrl, ResultsPageUp)
        , (Key.key (Key.Char 'f') `Key.withModifier` Key.Ctrl, ResultsPageDown)
        , (Key.key (Key.Char 'f'),  DisplayResultsOnly)
        , (Key.key Key.Tab,         SplitFocusPager) ]
    , _pagerKeybindings = KeybindingMap.fromList
        [ (Key.key Key.Up,          PagerUp)
        , (Key.key Key.Down,        PagerDown)
        , (Key.key Key.PageUp,      PagerPageUp)
        , (Key.key Key.PageDown,    PagerPageDown)
        , (Key.key Key.Left,        PagerScrollLeft)
        , (Key.key Key.Right,       PagerScrollRight)
        , (Key.key (Key.Char 'k'),  PagerUp)
        , (Key.key (Key.Char 'j'),  PagerDown)
        , (Key.key (Key.Char 'h'),  PagerScrollLeft)
        , (Key.key (Key.Char 'l'),  PagerScrollRight)
        , (Key.key (Key.Char 'u') `Key.withModifier` Key.Ctrl, PagerPageUp)
        , (Key.key (Key.Char 'd') `Key.withModifier` Key.Ctrl, PagerPageDown)
        , (Key.key (Key.Char 'b') `Key.withModifier` Key.Ctrl, PagerPageUp)
        , (Key.key (Key.Char 'f') `Key.withModifier` Key.Ctrl, PagerPageDown)
        , (Key.key (Key.Char 'f'),  DisplayPagerOnly)
        , (Key.key Key.Tab,         SplitFocusResults)
        , (Key.key (Key.Char 'q'),  DisplayResultsOnly) ]
    , _globalKeybindings = KeybindingMap.fromList
        [ (Key.key (Key.Char 'e'),  OpenFileInEditor)
        , (Key.key (Key.Char 'q'),  Exit) ]
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
