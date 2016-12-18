module Vgrep.Ansi.Type
  ( Formatted (..)
  , empty
  , bare
  , format
  , cat
  , mapText
  , mapTextWithPos
  ) where

import           Data.Foldable (foldl')
import           Data.Text     (Text)
import qualified Data.Text     as T
import           Prelude       hiding (length)


data Formatted attr
    = Empty
    | Text !Int Text
    | Format !Int attr (Formatted attr)
    | Cat !Int [Formatted attr]
    deriving (Eq, Show)

instance Functor Formatted where
    fmap f = \case
        Empty        -> Empty
        Text l t     -> Text l t
        Format l a t -> Format l (f a) (fmap f t)
        Cat l ts     -> Cat l (map (fmap f) ts)

instance (Eq attr, Monoid attr) => Monoid (Formatted attr) where
    mempty = Empty
    mappend = fuse


empty :: Formatted attr
empty = Empty

bare :: Text -> Formatted attr
bare t
    | T.null t  = empty
    | otherwise = Text (T.length t) t

format :: (Eq attr, Monoid attr) => attr -> Formatted attr -> Formatted attr
format attr formatted
    | attr == mempty = formatted
    | Format l attr' formatted' <- formatted
                     = Format l (attr `mappend` attr') formatted'
    | otherwise      = format' attr formatted

format' :: attr -> Formatted attr -> Formatted attr
format' attr formatted = Format (length formatted) attr formatted

cat :: (Eq attr, Monoid attr) => [Formatted attr] -> Formatted attr
cat = \case
    []  -> empty
    [t] -> t
    ts  -> foldl' fuse empty ts

cat' :: [Formatted attr] -> Formatted attr
cat' = \case
    []  -> empty
    [t] -> t
    ts  -> Cat (sum (fmap length ts)) ts

fuse :: (Eq attr, Monoid attr) => Formatted attr -> Formatted attr -> Formatted attr
fuse left right = case (left, right) of
    (Empty,           formatted)    -> formatted
    (formatted,       Empty)        -> formatted
    (Text l t,        Text l' t')   -> Text (l + l') (t `mappend` t')
    (Format l attr t, Format l' attr' t')
        | attr' == attr             -> Format (l + l') attr (t `mappend` t')

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



mapText :: (Text -> Text) -> Formatted a -> Formatted a
mapText f = \case
    Empty           -> empty
    Text _ t        -> bare (f t)
    Format _ attr t -> format' attr (mapText f t)
    Cat _ ts        -> cat' (map (mapText f) ts)

mapTextWithPos :: (Int -> Text -> Text) -> Formatted a -> Formatted a
mapTextWithPos f = go 0
  where
    go pos = \case
        Empty           -> empty
        Text _ t        -> bare (f pos t)
        Format _ attr t -> format' attr (go pos t)
        Cat _ ts        -> cat' (go2 pos ts)
    go2 pos = \case
        []     -> []
        t : ts -> let t'  = go pos t
                      l'  = length t'
                      ts' = go2 (pos + l') ts
                  in  t' : ts'
