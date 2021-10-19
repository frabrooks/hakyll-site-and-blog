
module SiteParser where

import Control.Applicative
import Control.Monad

class MonadTrans t where
  lift :: (Monad m) => m a -> t m a

newtype MaybeT m a = MaybeT
  { runMaybeT :: m (Maybe a)
  }

newtype Writer w a = Writer
  { runWriter :: (a, w)
  } deriving (Show)


newtype Parser a = Parser
  { runParser :: String -> MaybeT (Writer [String]) (String , a)
  }
-- Why runWriter/runParser? because runParser as a 'getter' has type:
-- runParser :: Parser a -> String -> m (String , a)
--
-- This is common pattern in haskell: record field with actionable
-- name i.e. runSomething / doSomething ... etc.



-- MaybeT instances
instance MonadTrans MaybeT where
  lift = MaybeT . liftM Just

instance (Functor m) => Functor (MaybeT m) where
  
  fmap f = MaybeT . (fmap (fmap f)) . runMaybeT
  -- NB (runMaybeT = getter, i.e. getMaybeT (:: m Maybe a))
  
instance (Functor m, Monad m) => Applicative (MaybeT m) where

  pure = lift . return

  mf <*> mx = MaybeT $ do
    mb_f <- runMaybeT mf
    mb_a <- runMaybeT mx
    return $ mb_f <*> mb_a



instance (Monad m) => Monad (MaybeT m) where
  return = lift . return
  x >>= f = MaybeT $ do
    v <- runMaybeT x
    case v of
      Nothing -> return Nothing
      Just y  -> runMaybeT (f y)

instance (Monad m) => Alternative (MaybeT m) where
  
  empty = MaybeT $ return Nothing

  (MaybeT m1) <|> (MaybeT m2) = MaybeT $ do (<|>) <$> m1 <*> m2


-- Writer intances
instance Functor (Writer w) where
  fmap f (Writer (a, w)) = Writer (f a, w)

instance (Monoid w) => Applicative (Writer w) where
  pure x = Writer (x, mempty)

  (Writer (f, log)) <*> (Writer (a, log'))
    = Writer (f a, log <> log')

instance (Monoid w) => Monad (Writer w) where
  return = pure
  (Writer (x, log)) >>= f
    = let (Writer (y, log')) = f x
      in Writer (y, log <> log')


-- Parser instances
instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \ input -> fmap (mapSnd f) (p input)
    where
      mapSnd :: (b -> c) -> (a, b) -> (a, c)
      mapSnd f (a, b) = (a, f b)
      
instance Applicative Parser where
  
  pure x = Parser $ \ input -> return (input , x)
  -- Pure (constant) parser / empty parser = just
  -- parse a given value without looking at input

  (Parser p1) <*> (Parser p2)
    = Parser $ \ input -> do
      (input', f) <- p1 input
      (input'', a) <- p2 input'
      return (input'', f a)

instance Monad Parser where

  return = pure

  (Parser p1) >>= fp
    = Parser $ \ input -> do
      (input', a) <- p1 input
      (input'', b) <- runParser (fp a) $ input'
      return (input'', b)

instance Alternative Parser where
  empty = Parser $ \ _ -> MaybeT $ return Nothing
  
  (Parser p1) <|> (Parser p2) = Parser $ \ input -> (p1 input) <|> p2 input
  

      
