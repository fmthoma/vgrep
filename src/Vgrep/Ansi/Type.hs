module Vgrep.Ansi.Type where


import Data.Text (Text)
import qualified Data.Text as T


data Formatted attr = Node attr [Formatted attr] | Leaf attr Text
    deriving (Eq, Show)

instance Functor Formatted where
    fmap f = \case
        Node attr ts -> Node (f attr) (map (fmap f) ts)
        Leaf attr t  -> Leaf (f attr) t

instance (Eq attr, Monoid attr) => Monoid (Formatted attr) where
    mempty = Leaf mempty T.empty

    Node attr ts `mappend` Leaf attr' t
        | T.null t           = Node attr  ts
        | null ts            = Leaf attr' t
        | attr  == mempty    = Node attr  (ts ++ [Leaf attr' t])
        | attr  == attr'     = Node attr  (ts ++ [Leaf mempty t])
    Leaf attr t  `mappend` Node attr' ts
        | T.null t           = Node attr' ts
        | null ts            = Leaf attr  t
        | attr' == mempty    = Node attr' (Leaf attr t   : ts)
        | attr' == attr      = Node attr' (Leaf mempty t : ts)
    Leaf attr t  `mappend` Leaf attr' t'
        | T.null t           = Leaf attr' t'
        | T.null t'          = Leaf attr  t
        | attr == attr'      = Leaf attr  (t `mappend` t')
    Node attr ts `mappend` Node attr' ts'
        | attr == attr'      = Node attr  (ts ++ ts')
    l            `mappend` r = Node mempty [l, r]

mapText :: (Text -> Text) -> Formatted a -> Formatted a
mapText f = \case
    Node attr ts -> Node attr (fmap (mapText f) ts)
    Leaf attr t  -> Leaf attr (f t)
