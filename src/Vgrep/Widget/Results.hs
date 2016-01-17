{-# LANGUAGE LambdaCase, Rank2Types, TemplateHaskell, MultiWayIf #-}
module Vgrep.Widget.Results
    ( ResultsState ()
    , ResultsWidget
    , resultsWidget

    , previousLine
    , nextLine

    , currentFileName
    , currentLineNumber

    -- Re-exports from Vgrep.Widget.Results.Buffer
    , File(..)
    , LineReference
    , FileLineReference
    ) where

import Control.Lens ( Getter
                    , view, to
                    , modifying, assign
                    , _1, _2 )
import Control.Lens.TH
import Control.Monad.State (State)
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Graphics.Vty
import Graphics.Vty.Prelude
import Prelude

import Vgrep.Widget.Type
import Vgrep.Widget.Results.Buffer as Buffer


data ResultsState = State { _files     :: Buffer
                          , _region    :: DisplayRegion }

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


previousLine :: State ResultsState ()
previousLine = modifying files (\fs -> fromMaybe fs (moveUp fs))

nextLine :: State ResultsState ()
nextLine = modifying files (\fs -> fromMaybe fs (moveUp fs))


drawResultList :: ResultsState -> Image
drawResultList state = drawLines width (toLines (view files state))
  where width = regionWidth (view region state)

drawLines :: Int -> [DisplayLine] -> Image
drawLines width ls = foldMap (drawLine (width, lineNumberWidth)) ls
  where lineNumberWidth = 5 -- FIXME

drawLine :: (Int, Int) -> DisplayLine -> Image
drawLine (width, lineNumberWidth) = \case
    FileHeader file             -> drawFileHeader file
    Line         (lineNumber, lineText) -> drawLineNumber lineNumber
                                       <|> drawLineText False lineText
    SelectedLine (lineNumber, lineText) -> drawLineNumber lineNumber
                                       <|> drawLineText True lineText
  where
    fileHeaderStyle = defAttr `withBackColor` green
    lineNumberStyle = defAttr `withForeColor` brightBlack
                              `withBackColor` black
    resultLineStyle = defAttr
    highlightStyle  = defAttr `withStyle` standout

    padWithSpace w s = T.take (fromIntegral w)
                     . T.justifyLeft (fromIntegral w) ' '
                     $ ' ' `T.cons` s
    justifyRight w s = replicate (w - length s - 1) ' ' ++ s ++ " "

    drawFileHeader :: File -> Image
    drawFileHeader = text fileHeaderStyle . padWithSpace width . getFileName

    drawLineNumber :: Maybe Int -> Image
    drawLineNumber = string lineNumberStyle
                   . justifyRight lineNumberWidth
                   . maybe "" show

    drawLineText :: Bool -> Text -> Image
    drawLineText isCurrent = text style
                         . padWithSpace (width - lineNumberWidth)
      where style = if isCurrent then resultLineStyle <> highlightStyle
                                 else resultLineStyle


resizeToRegion :: DisplayRegion -> State ResultsState ()
resizeToRegion newRegion = do
    assign region newRegion
    modifying files (Buffer.resize (regionHeight newRegion))


currentFileName :: Getter ResultsWidget Text
currentFileName = widgetState . files . to current . _1 . to getFileName

currentLineNumber :: Getter ResultsWidget (Maybe Int)
currentLineNumber = widgetState . files . to current . _2 . _1
