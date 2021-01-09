module Vgrep.Ansi.Type
  ( Formatted (..)
  , AnsiFormatted
  -- * Smart constructors
  , emptyFormatted
  , bare
  , format
  , cat
  -- * Modifying the underlying text
  , mapText
  , mapTextWithPos
  , takeFormatted
  , dropFormatted
  , padFormatted
  -- * Internal helpers
  , fuse
  ) where

import           Data.Foldable (foldl')
import           Data.Text     (Text)
import qualified Data.Text     as T
import           Graphics.Vty  (Attr)
import           Prelude       hiding (length)


-- | A representattion of formatted 'Text'. The attribute is usually a 'Monoid'
-- so that different formattings can be combined by nesting them.
data Formatted attr
    = Empty
    -- ^ An empty block

    | Text !Int Text
    -- ^ A bare (unformatted) text node

    | Format !Int attr (Formatted attr)
    -- ^ Adds formatting to a block

    | Cat !Int [Formatted attr]
    -- ^ Concatenates several blocks of formatted text

    deriving (Eq, Show)

instance Functor Formatted where
    fmap f = \case
        Empty        -> Empty
        Text l t     -> Text l t
        Format l a t -> Format l (f a) (fmap f t)
        Cat l ts     -> Cat l (map (fmap f) ts)

instance (Eq attr, Semigroup attr) => Semigroup (Formatted attr) where
    (<>) = fuse

instance (Eq attr, Semigroup attr) => Monoid (Formatted attr) where
    mempty = Empty


-- | Type alias for Text formatted with 'Attr' from "Graphics.Vty".
type AnsiFormatted = Formatted Attr


-- | Smart constructor for an empty 'Formatted' text.
emptyFormatted :: Formatted attr
emptyFormatted = Empty

-- | Smart constructor for bare (unformatted) text.
--
-- >>> bare ""
-- Empty
--
-- >>> bare "Text"
-- Text 4 "Text"
--
bare :: Text -> Formatted attr
bare t
    | T.null t  = emptyFormatted
    | otherwise = Text (T.length t) t

-- | Adds formatting to a 'Formatted' text. The 'Eq' and 'Monoid' instances for
-- @attr@ are used to flatten redundant formattings.
--
-- >>> format (Just ()) (format Nothing (bare "Text"))
-- Format 4 (Just ()) (Text 4 "Text")
--
-- >>> format (Just ()) (format (Just ()) (bare "Text"))
-- Format 4 (Just ()) (Text 4 "Text")
--
-- >>> format Nothing (bare "Text")
-- Text 4 "Text"
--
format :: (Eq attr, Monoid attr) => attr -> Formatted attr -> Formatted attr
format attr formatted
    | attr == mempty = formatted
    | Format l attr' formatted' <- formatted
                     = Format l (attr <> attr') formatted'
    | otherwise      = format' attr formatted

format' :: attr -> Formatted attr -> Formatted attr
format' attr formatted = Format (length formatted) attr formatted

-- | Concatenates pieces of 'Formatted' text. Redundant formattings and blocks
-- of equal formatting are 'fuse'd together.
cat :: (Eq attr, Monoid attr) => [Formatted attr] -> Formatted attr
cat = \case
    []  -> emptyFormatted
    [t] -> t
    ts  -> foldl' fuse emptyFormatted ts

cat' :: [Formatted attr] -> Formatted attr
cat' = \case
    []  -> emptyFormatted
    [t] -> t
    ts  -> Cat (sum (fmap length ts)) ts

-- | Simplifies 'Formatted' text by leaving out redundant empty bits, joining
-- pieces of text with the same formatting, and flattening redundant
-- applications of the same style.
--
-- >>> emptyFormatted `fuse` bare "Text"
-- Text 4 "Text"
--
-- >>> format (Just ()) (bare "Left") `fuse` format (Just ()) (bare "Right")
-- Format 9 (Just ()) (Text 9 "LeftRight")
--
fuse :: (Eq attr, Semigroup attr) => Formatted attr -> Formatted attr -> Formatted attr
fuse left right = case (left, right) of
    (Empty,           formatted)    -> formatted
    (formatted,       Empty)        -> formatted
    (Text l t,        Text l' t')   -> Text (l + l') (t <> t')
    (Format l attr t, Format l' attr' t')
        | attr' == attr             -> Format (l + l') attr (t <> t')

    (Cat l ts,        Cat l' ts')   -> Cat (l + l') (ts ++ ts')
    (Cat l ts,        formatted)    -> Cat (l + length formatted) (ts ++ [formatted])
    (formatted,       Cat _ (t:ts)) -> case formatted `fuse` t of
                                          Cat _ ts' -> cat' (ts' ++ ts)
                                          t'        -> cat' (t' : ts)
    (formatted,     formatted')     -> cat' [formatted, formatted']

length :: Formatted attr -> Int
length = \case
    Empty        -> 0
    Text   l _   -> l
    Format l _ _ -> l
    Cat    l _   -> l



-- | Apply a function to each piece of text in the 'Formatted' tree.
--
-- >>> mapText T.toUpper (Cat 11 [Text 6 "Hello ", Format 5 () (Text 5 "World")])
-- Cat 11 [Text 6 "HELLO ",Format 5 () (Text 5 "WORLD")]
--
mapText :: (Text -> Text) -> Formatted a -> Formatted a
mapText f = \case
    Empty           -> emptyFormatted
    Text _ t        -> bare (f t)
    Format _ attr t -> format' attr (mapText f t)
    Cat _ ts        -> cat' (map (mapText f) ts)

-- | Like 'mapText', but passes the position of the text chunk to the function
-- as well. Can be used for formatting text position-dependently, e.g. for
-- expanding tabs to spaces.
mapTextWithPos :: (Int -> Text -> Text) -> Formatted a -> Formatted a
mapTextWithPos f = go 0
  where
    go pos = \case
        Empty           -> emptyFormatted
        Text _ t        -> bare (f pos t)
        Format _ attr t -> format' attr (go pos t)
        Cat _ ts        -> cat' (go2 pos ts)
    go2 pos = \case
        []     -> []
        t : ts -> let t'  = go pos t
                      l'  = length t'
                      ts' = go2 (pos + l') ts
                  in  t' : ts'


-- | Crops the text to a given length. If the text is already shorter than the
-- desired width, it is returned as-is.
takeFormatted :: Int -> Formatted a -> Formatted a
takeFormatted w txt
    | length txt > w  = mapTextWithPos cropChunk txt
    | otherwise       = txt
  where
    cropChunk pos
        | pos >= w   = const T.empty
        | otherwise  = T.take (w - pos)

-- | Drops a prefix of the given length. If the text is already shorter than the
-- number of characters to be dropped, 'emptyFormatted' is returned.
dropFormatted :: Int -> Formatted a -> Formatted a
dropFormatted amount txt
    | amount <= 0          = txt
    | length txt < amount  = emptyFormatted
    | otherwise = case txt of
        Empty           -> emptyFormatted
        Text _ t        -> bare (T.drop amount t)
        Format _ attr t -> format' attr (dropFormatted amount t)
        Cat _ ts        -> cat' (dropChunks amount ts)
  where
    dropChunks n = \case
        []   -> []
        t:ts -> dropFormatted n t : dropChunks (n - length t) ts

-- | Pads the text to a given width. If the text is already longer than the
-- desired width, it is returned as-is.
padFormatted :: Int -> Char -> Formatted a -> Formatted a
padFormatted w c txt
    | w > length txt  = cat' [txt, padding (w - length txt)]
    | otherwise       = txt
  where
    padding l = bare (T.replicate l (T.singleton c))
