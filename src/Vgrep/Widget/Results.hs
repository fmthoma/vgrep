{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module Vgrep.Widget.Results
    ( ResultsState ()
    , ResultsWidget
    , resultsWidget

    , prevLine
    , nextLine
    , pageUp
    , pageDown

    , currentFileName
    , currentLineNumber

    , module Vgrep.Results
    ) where

import Control.Applicative
import Control.Lens
import Control.Monad.State.Extended
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Graphics.Vty hiding ((<|>))
import Graphics.Vty.Prelude
import Prelude

import Vgrep.Type
import Vgrep.Widget.Type
import Vgrep.Results
import Vgrep.Results.Buffer as Buffer


data ResultsState = State { _files  :: Buffer
                          , _region :: DisplayRegion }

makeLenses ''ResultsState


type ResultsWidget = Widget ResultsState

resultsWidget :: DisplayRegion
              -> [FileLineReference]
              -> ResultsWidget
resultsWidget initialDimensions fileResults =
    Widget { _widgetState = initState fileResults initialDimensions
           , _dimensions  = initialDimensions
           , _resize      = resizeToRegion
           , _draw        = drawResultList }

initState :: [FileLineReference]
          -> DisplayRegion
          -> ResultsState
initState fileResults initialDimensions =
    let Just foo = buffer fileResults -- FIXME handle case of empty input
    in  State { _files  = Buffer.resize (regionHeight initialDimensions) foo
              , _region = initialDimensions }

pageUp, pageDown :: State ResultsState ()
pageUp = do
    unlessS (isJust . moveUp . view files) $ do
        modifying files (repeatedly (hideNext >=> showPrev))
        resizeToWindow
    modifying files (repeatedly moveUp)
pageDown = do
    unlessS (isJust . moveDown . view files) $ do
        modifying files (repeatedly hidePrev)
        resizeToWindow
    modifying files (repeatedly moveDown)

repeatedly :: (a -> Maybe a) -> a -> a
repeatedly f = go
  where
    go x | Just x' <- f x = go x'
         | otherwise      = x


prevLine, nextLine :: State ResultsState ()
prevLine = zoom files (maybeModify tryPrevLine) >> resizeToWindow
nextLine = zoom files (maybeModify tryNextLine) >> resizeToWindow

tryPrevLine, tryNextLine :: Buffer -> Maybe Buffer
tryPrevLine buf = moveUp   buf <|> (showPrev buf >>= tryPrevLine)
tryNextLine buf = moveDown buf <|> (showNext buf >>= tryNextLine)

maybeModify :: (s -> Maybe s) -> State s ()
maybeModify f = do
    s <- get
    case f s of
        Just s' -> put s'
        Nothing -> pure ()


drawResultList :: ResultsState -> Vgrep Image
drawResultList s = pure (drawLines width (toLines (view files s)))
  where width = regionWidth (view region s)

drawLines :: Int -> [DisplayLine] -> Image
drawLines width ls = foldMap (drawLine (width, lineNumberWidth)) ls
  where lineNumberWidth = foldl' max 0
                        . map (twoExtraSpaces . length . show)
                        . catMaybes
                        $ map lineNumber ls
        twoExtraSpaces = (+ 2)

drawLine :: (Int, Int) -> DisplayLine -> Image
drawLine (width, lineNumberWidth) = \case
    FileHeader file     -> drawFileHeader file
    Line         (n, t) -> horizCat [drawLineNumber n, drawLineText t]
    SelectedLine (n, t) -> horizCat [drawLineNumber n, drawSelectedLineText t]
  where
    fileHeaderStyle = defAttr `withBackColor` green
    lineNumberStyle = defAttr `withForeColor` blue
    resultLineStyle = defAttr
    highlightStyle  = defAttr `withStyle` standout

    padWithSpace w = T.take (fromIntegral w)
                   . T.justifyLeft (fromIntegral w) ' '
                   . T.cons ' '
    justifyRight w s =
        let padAmount = fromIntegral w - T.length s - 1
        in T.replicate padAmount " " <> s <> " "

    drawFileHeader :: File -> Image
    drawFileHeader = text fileHeaderStyle . padWithSpace width . getFileName

    drawLineNumber :: Maybe Int -> Image
    drawLineNumber = text lineNumberStyle
                   . justifyRight lineNumberWidth
                   . maybe "" (T.pack . show)

    drawSelectedLineText :: Text -> Image
    drawSelectedLineText = text highlightStyle
                         . padWithSpace (width - lineNumberWidth)

    drawLineText :: Text -> Image
    drawLineText = text resultLineStyle
                 . padWithSpace (width - lineNumberWidth)


resizeToRegion :: DisplayRegion -> State ResultsState ()
resizeToRegion newRegion = do
    assign region newRegion
    resizeToWindow

resizeToWindow :: State ResultsState ()
resizeToWindow = do
    height <- uses region regionHeight
    modifying files (Buffer.resize height)


currentFileName :: Getter ResultsWidget Text
currentFileName = widgetState . files . to current . _1 . to getFileName

currentLineNumber :: Getter ResultsWidget (Maybe Int)
currentLineNumber = widgetState . files . to current . _2 . _1
