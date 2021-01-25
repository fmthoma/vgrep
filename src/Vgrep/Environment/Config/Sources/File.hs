{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-} -- Because of camelTo
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Vgrep.Environment.Config.Sources.File
    ( configFromFile
    , Attr
    , Color
    , Style
    ) where

import           Control.Monad           ((<=<))
import           Control.Monad.IO.Class
import           Data.Aeson.Types
    ( FromJSON (..)
    , Options (..)
    , Parser
    , camelTo
    , defaultOptions
    , genericParseJSON
    , withObject
    , (.!=)
    , (.:?)
    )
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as M
import           Data.Maybe
import           Data.Monoid
import           Data.Yaml.Aeson
    ( decodeFileEither
    , prettyPrintParseException
    )
import           GHC.Generics
import qualified Graphics.Vty.Attributes as Vty
import           System.Directory
import           System.IO
import           Text.Read               (readMaybe)

import           Vgrep.Command
import           Vgrep.Environment.Config.Monoid
import qualified Vgrep.Key                       as Key
import           Vgrep.KeybindingMap

-- $setup
-- >>> import Data.Either (isLeft)
-- >>> import Data.Yaml.Aeson (decodeEither', ParseException(..))


{- |
Reads the configuration from a JSON or YAML file. The file should be
located in one of the following places:

* @~\/.vgrep\/config.yaml@,
* @~\/.vgrep\/config.yml@,
* @~\/.vgrep\/config.json@ or
* @~\/.vgrep\/config@.

When none of these files exist, no error is raised. When a file exists, but
cannot be parsed, a warning is written to stderr.

Supported formats are JSON and YAML. The example YAML config given in the
project directory (@config.yaml.example@) is equivalent to the default
config:

>>> import qualified Vgrep.Environment.Config as C
>>> Right config <- decodeFileEither "config.yaml.example" :: IO (Either ParseException ConfigMonoid)
>>> C.fromConfigMonoid config == C.defaultConfig
True

Example YAML config file for 'Vgrep.Environment.Config.defaultConfig':

> colors:
>   line-numbers:
>     fore-color: blue
>   line-numbers-hl:
>     fore-color: blue
>     style: bold
>   normal: {}
>   normal-hl:
>     style: bold
>   file-headers:
>     back-color: green
>   selected:
>     style: standout
> tabstop: 8
> editor: "vi"

Example JSON file for the same config:

> {
>   "colors": {
>     "line-numbers" : {
>       "fore-color": "blue"
>     },
>     "line-numbers-hl": {
>       "fore-color": "blue",
>       "style": "bold"
>     },
>     "normal": {},
>     "normal-hl": {
>       "style": "bold"
>     },
>     "file-headers": {
>       "back-color": "green"
>     },
>     "selected": {
>       "style": "standout"
>     }
>   },
>   "tabstop": 8,
>   "editor": "vi"
> }

The JSON/YAML keys correspond to the lenses in "Vgrep.Environment.Config",
the values for 'Vty.Color' and 'Vty.Style' can be obtained from the
corresponding predefined constants in "Graphics.Vty.Attributes".
-}
configFromFile :: MonadIO io => io ConfigMonoid
configFromFile = liftIO $ do
    configDir <- getAppUserDataDirectory "vgrep"
    let configFiles = map (configDir </>)
            [ "config.yaml"
            , "config.yml"
            , "config.json"
            , "config" ]
    findExistingFile configFiles >>= \case
        Nothing         -> pure mempty
        Just configFile -> decodeFileEither configFile >>= \case
            Right config -> pure config
            Left err     -> do
                hPutStrLn stderr $
                    "Could not parse config file " ++ configFile ++ ":"
                    ++ "\n" ++ prettyPrintParseException err
                    ++ "\nFalling back to default config."
                pure mempty
  where
    findExistingFile :: [FilePath] -> IO (Maybe FilePath)
    findExistingFile = \case
        [] -> pure Nothing
        f : fs -> do
            exists <- doesFileExist f
            if exists then pure (Just f) else findExistingFile fs

    (</>) :: FilePath -> FilePath -> FilePath
    dir </> file = dir <> "/" <> file


instance FromJSON ConfigMonoid where
    parseJSON = withObject "ConfigMonoid" $ \o -> do
        _mcolors  <- o .:? "colors" .!= mempty
        _mtabstop <- fmap First (o .:? "tabstop")
        _meditor  <- fmap First (o .:? "editor")
        _mkeybindings <- o .:? "keybindings" .!= mempty
        pure ConfigMonoid{..}

instance FromJSON ColorsMonoid where
    parseJSON = genericParseJSON jsonOptions

instance FromJSON Vty.Attr where
    parseJSON = fmap attrToVty . parseJSON


{- |
A JSON-parsable data type for 'Vty.Attr'.

JSON example:

>>> decodeEither' "{\"fore-color\": \"black\", \"style\": \"standout\"}" :: Either ParseException Attr
Right (Attr {foreColor = Just Black, backColor = Nothing, style = Just Standout})

JSON example without quotes:
>>> decodeEither' "{fore-color: black, style: standout}" :: Either ParseException Attr
Right (Attr {foreColor = Just Black, backColor = Nothing, style = Just Standout})

YAML example:

>>> :{
>>> decodeEither'
>>>   $  "fore-color: \"blue\"\n"
>>>   <> "back-color: \"bright-blue\"\n"
>>>   <> "style: \"reverse-video\"\n"
>>>   :: Either ParseException Attr
>>> :}
Right (Attr {foreColor = Just Blue, backColor = Just BrightBlue, style = Just ReverseVideo})

YAML example without quotes:

>>> :{
>>> decodeEither'
>>>   $  "fore-color: blue\n"
>>>   <> "back-color: bright-blue\n"
>>>   <> "style: reverse-video\n"
>>>   :: Either ParseException Attr
>>> :}
Right (Attr {foreColor = Just Blue, backColor = Just BrightBlue, style = Just ReverseVideo})

An empty JSON/YAML object yields the default colors:

>>> decodeEither' "{}" :: Either ParseException Attr
Right (Attr {foreColor = Nothing, backColor = Nothing, style = Nothing})
-}
data Attr = Attr
    { foreColor :: Maybe Color
    , backColor :: Maybe Color
    , style     :: Maybe Style
    }
    deriving (Eq, Show, Generic)

instance FromJSON Attr where
    parseJSON = genericParseJSON jsonOptions

attrToVty :: Attr -> Vty.Attr
attrToVty Attr{..} = foldAttrs
    [ fmap (flip Vty.withForeColor . colorToVty) foreColor
    , fmap (flip Vty.withBackColor . colorToVty) backColor
    , fmap (flip Vty.withStyle     . styleToVty) style ]
  where
    foldAttrs = foldr ($) Vty.defAttr . catMaybes


{- |
A JSON-parsable data type for 'Vty.Color'.

>>> decodeEither' "[\"black\",\"red\",\"bright-black\"]" :: Either ParseException [Color]
Right [Black,Red,BrightBlack]

Also works without quotes:

>>> decodeEither' "[black,red,bright-black]" :: Either ParseException [Color]
Right [Black,Red,BrightBlack]

Fails with error message if the 'Color' cannot be parsed:

>>> isLeft (decodeEither' "foo" :: Either ParseException Color)
True
-}
data Color
    = Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White
    | BrightBlack
    | BrightRed
    | BrightGreen
    | BrightYellow
    | BrightBlue
    | BrightMagenta
    | BrightCyan
    | BrightWhite
    deriving (Eq, Show, Generic)

instance FromJSON Color where
    parseJSON = genericParseJSON jsonOptions

colorToVty :: Color -> Vty.Color
colorToVty = \case
    Black         -> Vty.black
    Red           -> Vty.red
    Green         -> Vty.green
    Yellow        -> Vty.yellow
    Blue          -> Vty.blue
    Magenta       -> Vty.magenta
    Cyan          -> Vty.cyan
    White         -> Vty.white
    BrightBlack   -> Vty.brightBlack
    BrightRed     -> Vty.brightRed
    BrightGreen   -> Vty.brightGreen
    BrightYellow  -> Vty.brightYellow
    BrightBlue    -> Vty.brightBlue
    BrightMagenta -> Vty.brightMagenta
    BrightCyan    -> Vty.brightCyan
    BrightWhite   -> Vty.brightWhite


{- |
A JSON-parsable data type for 'Vty.Style'.

>>> decodeEither' "[\"standout\", \"underline\", \"bold\"]" :: Either ParseException [Style]
Right [Standout,Underline,Bold]

Also works without quotes:

>>> decodeEither' "[standout, underline, bold]" :: Either ParseException [Style]
Right [Standout,Underline,Bold]

Fails with error message if the 'Style' cannot be parsed:

>>> isLeft (decodeEither' "foo" :: Either ParseException Color)
True
-}
data Style
    = Standout
    | Underline
    | ReverseVideo
    | Blink
    | Dim
    | Bold
    deriving (Eq, Show, Generic)

instance FromJSON Style where
    parseJSON = genericParseJSON jsonOptions

styleToVty :: Style -> Vty.Style
styleToVty = \case
    Standout     -> Vty.standout
    Underline    -> Vty.underline
    ReverseVideo -> Vty.reverseVideo
    Blink        -> Vty.blink
    Dim          -> Vty.dim
    Bold         -> Vty.bold


instance FromJSON KeybindingsMonoid where
    parseJSON = genericParseJSON jsonOptions

instance FromJSON Command where
    parseJSON = genericParseJSON jsonOptions

instance FromJSON KeybindingMap where
    parseJSON = fmap KeybindingMap . mapMKeys parseChord <=< parseJSON

mapMKeys :: (Monad m, Ord k') => (k -> m k') -> Map k v -> m (Map k' v)
mapMKeys f = fmap M.fromList . M.foldrWithKey go (pure [])
  where
    go k x mxs = do
        k' <- f k
        xs <- mxs
        pure ((k', x) : xs)

parseChord :: String -> Parser Key.Chord
parseChord = \case
    'C' : '-' : t -> fmap (`Key.withModifier` Key.Ctrl)  (parseChord t)
    'S' : '-' : t -> fmap (`Key.withModifier` Key.Shift) (parseChord t)
    'M' : '-' : t -> fmap (`Key.withModifier` Key.Meta)  (parseChord t)
    [c]           -> pure (Key.key (Key.Char c))
    "PgUp"        -> pure (Key.key Key.PageUp)
    "PgDown"      -> pure (Key.key Key.PageDown)
    "PgDn"        -> pure (Key.key Key.PageDown)
    s | Just k <- readMaybe s
                  -> pure (Key.key k)
      | otherwise -> fail ("Unknown key '" <> s <> "'")

jsonOptions :: Options
jsonOptions = defaultOptions
    { constructorTagModifier = camelTo '-'
    , fieldLabelModifier     = camelTo '-' . stripPrefix }
  where
    stripPrefix = \case
        '_' : 'm' : name -> name
        '_' : name       -> name
        name             -> name
