module Vgrep.Ansi.Type
  ( Formatted (..)
  , empty
  , bare
  , format
  , cat
  , mapText
  ) where


import           Data.Text (Text)
import qualified Data.Text as T


data Formatted attr
    = Empty
    | Text Text
    | Format attr (Formatted attr)
    | Cat [Formatted attr]
    deriving (Eq, Show)

instance Functor Formatted where
    fmap f = \case
        Empty      -> Empty
        Text t     -> Text t
        Format a t -> Format (f a) (fmap f t)
        Cat ts     -> Cat (map (fmap f) ts)

instance (Eq attr, Monoid attr) => Monoid (Formatted attr) where
    mempty = Empty

    Empty         `mappend` formatted     = formatted
    formatted     `mappend` Empty         = formatted
    Text t        `mappend` Text t'       = Text (t `mappend` t')
    Format attr t `mappend` Format attr' t'
        | attr' == attr                   = Format attr (t `mappend` t')

    Cat ts        `mappend` Cat ts'       = Cat (ts ++ ts')
    Cat ts        `mappend` formatted     = Cat (ts ++ [formatted])
    formatted     `mappend` Cat (t:ts)    = case formatted `mappend` t of
                                              Cat ts' -> Cat (ts' ++ ts)
                                              t'      -> Cat (t' : ts)
    formatted     `mappend` formatted'    = Cat [formatted, formatted']


empty :: Formatted attr
empty = Empty

bare :: Text -> Formatted attr
bare t
    | T.null t  = empty
    | otherwise = Text t

format :: Monoid attr => attr -> Formatted attr -> Formatted attr
format attr formatted
    | Format attr' formatted' <- formatted
                = format (attr `mappend` attr') formatted'
    | otherwise = Format attr formatted

cat :: (Eq attr, Monoid attr) => [Formatted attr] -> Formatted attr
cat = \case
    []  -> empty
    [t] -> t
    ts  -> mconcat ts


mapText :: (Text -> Text) -> Formatted a -> Formatted a
mapText f = \case
    Empty         -> Empty
    Text t        -> Text (f t)
    Format attr t -> Format attr (mapText f t)
    Cat ts        -> Cat (map (mapText f) ts)
