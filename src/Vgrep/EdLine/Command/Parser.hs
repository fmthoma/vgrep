{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
module Vgrep.EdLine.Command.Parser (
      CmdParserT ()
    , parse
    , complete

    , word
    , select
    , anytoken
    , shortOption
    , longOption
    ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Class
import           Data.Monoid
import           Data.Text                 (Text)
import qualified Data.Text                 as Text


newtype CmdParserT m a = CmdParserT (forall b. (a -> m (NextT m b)) -> m (NextT m b))
    deriving (Functor)

instance Monad m => Applicative (CmdParserT m) where
    pure a = CmdParserT ($ a)
    (<*>) = ap

instance Monad m => Alternative (CmdParserT m) where
    empty = CmdParserT (const (pure empty))
    CmdParserT cont1 <|> CmdParserT cont2 = CmdParserT (\next -> liftA2 (<|>) (cont1 next) (cont2 next))

instance Monad m => Monad (CmdParserT m) where
    CmdParserT m >>= f = CmdParserT (\k -> m (\a -> let CmdParserT m' = f a in m' k))
    fail = pfail . Text.pack

instance MonadTrans CmdParserT where
    lift ma = CmdParserT (ma >>=)


data NextT m a
    = Get [Text] (Text -> m (NextT m a))
    | Pure a
    | Fail Text
    deriving (Functor)

instance Monad m => Applicative (NextT m) where
    pure = Pure
    (<*>) = ap

instance Monad m => Alternative (NextT m) where
    empty = Fail "No alternative"

    Fail err  <|> Fail _    = Fail err
    Fail _    <|> other     = other
    other     <|> Fail _    = other

    Get c1 f1 <|> Get c2 f2 = Get (c1 <> c2) (\t -> liftA2 (<|>) (f1 t) (f2 t))
    Get c  f  <|> Pure a    = Get c (\t -> liftA2 (<|>) (f t) (pure (Pure a)))

    Pure a    <|> _         = Pure a

instance Monad m => Monad (NextT m) where
    Fail e        >>= _ = Fail e
    Pure a        >>= f = f a
    Get comp next >>= f = Get comp $ \txt -> fmap (>>= f) (next txt)

    fail = Fail . Text.pack

instance MonadTrans NextT where
    lift ma = Get [] (\_ -> fmap pure ma)

pfail :: Monad m => Text -> CmdParserT m a
pfail err = CmdParserT (const (pure (Fail err)))


runNextT :: Monad m => NextT m a -> [Text] -> m (Either Text a)
runNextT parser tokens = case parser of
    Fail e -> pure (Left e)
    Pure a -> pure (Right a)
    Get _ next -> case tokens of
        []              -> emptyInput
        token : tokens' -> next token >>= \case
            Pure _     | not (null tokens') -> unexpectedTokens tokens'
            Get comp _ | null tokens'       -> unexpectedEndOfInput comp
            next'                           -> runNextT next' tokens'
  where
    unexpectedEndOfInput = pure . Left . \case
        []   -> "Premature end of input"
        comp -> "Premature end of input. Expected: " <> Text.intercalate " | " comp
    unexpectedTokens tokens' = pure (Left ("Unexpected \"" <> Text.unwords tokens' <> "\""))
    emptyInput = pure (Left "Empty input")

compNextT :: Monad m => NextT m a -> [Text] -> m [Text]
compNextT parser tokens = case parser of
    Get comp next -> case tokens of
        []              -> pure comp
        [lastToken]     -> pure (filter (lastToken `Text.isPrefixOf`) comp)
        token : tokens' -> next token >>= \next' -> compNextT next' tokens'
    _otherwise -> pure []


parse :: Monad m => CmdParserT m a -> Text -> m (Either Text a)
parse (CmdParserT next) text = do
    final <- next (pure . pure)
    runNextT final (Text.words text)

complete :: Monad m => CmdParserT m a -> Text -> m [Text]
complete (CmdParserT next) text = do
    final <- next (pure . pure)
    compNextT final (Text.words text)


anytoken :: Applicative m => CmdParserT m Text
anytoken = CmdParserT (pure . Get [])

word :: Monad m => Text -> CmdParserT m Text
word w = do
    w' <- CmdParserT (pure . Get [w])
    if w == w'
        then pure w
        else pfail ("Unexpected " <> w' <> ", expected: " <> w)

select :: Monad m => m [Text] -> CmdParserT m Text
select choices = do
    items <- lift choices
    token <- CmdParserT (pure . Get items)
    if token `elem` items
        then pure token
        else pfail ("Unexpected " <> token <> ", expected: " <> Text.intercalate " | " items)

-- file :: MonadIO m => CmdParserT m Text
-- file = select _

arg :: Applicative m => CmdParserT m Text
arg = anytoken

shortOption :: Monad m => Char -> CmdParserT m Text
shortOption opt = word ("-" <> Text.singleton opt) >> arg

longOption :: Monad m => Text -> CmdParserT m Text
longOption opt = word ("--" <> opt) >> arg
