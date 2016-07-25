{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE RecordWildCards            #-}
module Vgrep.EdLine (
    ) where

import           Control.Applicative
import           Control.Monad.Cont
import           Control.Monad.Cont.Class
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Monoid
import           Data.Text                 (Text)
import qualified Data.Text                 as Text

import Vgrep.Type


newtype CmdParserT m a = CmdParserT (forall b. (a -> m (NextT m b)) -> m (NextT m b))
    deriving (Functor)

instance Monad m => Applicative (CmdParserT m) where
    pure a = CmdParserT ($ a)

instance Monad m => Monad (CmdParserT m) where
    CmdParserT m >>= f = CmdParserT (\k -> m (\a -> let CmdParserT m' = f a in m' k))
    fail err = CmdParserT (const (pure (fail err)))

instance MonadTrans CmdParserT where
    lift ma = CmdParserT (ma >>=)


data NextT (m :: * -> *) a
    = Get (m [Text]) (Text -> m (NextT m a))
    | Pure a
    | Fail Text
    deriving (Functor)

instance Monad m => Applicative (NextT m) where
    pure = Pure
    (<*>) = ap

instance (Monad m, Alternative m) => Alternative (NextT m) where
    empty = Fail "No alternative" --FIXME

    Fail err1 <|> Fail err2 = Fail (Text.unlines [err1, err2])
    Fail _ <|> other = other
    other <|> Fail _ = other

    Get c1 f1 <|> Get c2 f2 = Get (liftA2 mappend c1 c2)(\t -> f1 t <|> f2 t)

instance Monad m => Monad (NextT m) where
    Fail e        >>= _ = Fail e
    Pure a        >>= f = f a
    Get comp next >>= f = Get comp $ \txt -> fmap (>>= f) (next txt)

    fail = Fail . Text.pack

instance MonadTrans NextT where
    lift ma = Get (pure []) (\_ -> fmap pure ma)


runNextT :: Monad m => NextT m a -> [Text] -> m (Either Text a)
runNextT next tokens = case next of
    Fail e -> pure (Left e)
    Pure a -> pure (Right a)
    Get comp next -> case tokens of
        [] -> pure (Left "FIXME")
        token : tokens' -> next token >>= \next' -> runNextT next' tokens'

compNextT :: Monad m => NextT m a -> [Text] -> m [Text]
compNextT next tokens = case next of
    Get enum next -> case tokens of
        [] -> enum
        [lastToken] -> fmap (filter (lastToken `Text.isPrefixOf`)) enum
        token : tokens' -> next token >>= \next' -> compNextT next' tokens
    _otherwise      -> pure []


anytoken :: Applicative m => CmdParserT m Text
anytoken = CmdParserT (pure . Get (pure []))

word :: Monad m => Text -> CmdParserT m Text
word w = do
    w' <- anytoken
    if w == w' then pure w else fail "FIXME"

select :: Monad m => m [Text] -> CmdParserT m Text
select choices = do
    items <- lift choices
    token <- anytoken
    if token `elem` items then pure token else fail "FIXME"

file :: MonadIO m => CmdParserT m Text
file = select _

arg :: Applicative m => CmdParserT m Text
arg = anytoken

shortOption :: Monad m => Char -> CmdParserT m Text
shortOption opt = void (select (pure [ "-" <> Text.singleton opt ])) >> arg

longOption :: Monad m => Text -> CmdParserT m Text
longOption opt = void (select (pure [ "--" <> opt ])) >> arg


parse :: Monad m => CmdParserT m a -> Text -> m (Either Text a)
parse = _

complete :: Monad m => CmdParserT m a -> Text -> m [Text]
complete = _
