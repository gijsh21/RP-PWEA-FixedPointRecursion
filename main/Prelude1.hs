{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE EmptyCase #-}
module Prelude1 where

data Free f a = Pure a | Op (f (Free f a))

infixr 6 +
data (f + g) a
  = L (f a)
  | R (g a)
  deriving Functor

data Err k = Err String
  deriving Functor

data End k -- No constructors!
  deriving Functor

class f < g where
  inj :: f k -> g k

instance f < f where inj = id
instance f < (f + g) where inj = L
instance {-# OVERLAPPABLE #-} f < h => f < (g + h) where inj = R . inj

instance Functor f => Monad (Free f) where
  m >>= k = fold k Op m

instance Functor f => Functor (Free f) where
  fmap f = fold (pure . f) Op

instance Functor f => Applicative (Free f) where
  pure = Pure
  f <*> m = fold (flip fmap m) Op f

fold :: Functor f => (a -> b) -> (f b -> b) -> Free f a -> b
fold gen _   (Pure x) = gen x
fold gen alg (Op f)   = alg (fmap (fold gen alg) f)

data Handler f a f' b
  = Handler { ret  :: a -> Free f' b
            , hdlr :: f (Free f' b) -> Free f' b }

handle :: (Functor f, Functor f')
       => Handler f a f' b -> Free (f + f') a -> Free f' b
handle h = fold
  (ret h)
  (\x -> case x of
     L y -> hdlr h y
     R y -> Op y)

infixr 5 ->:
type (f ->: g) = forall a. f a -> g a

assocSum :: f + (g + h) ->: (f + g) + h
assocSum (L x) = L (L x)
assocSum (R (L x)) = L (R x)
assocSum (R (R x)) = R x

commSum :: f + g ->: g + f
commSum (L x) = R x
commSum (R x) = L x

permute :: (Functor f, Functor f')
        => (f ->: f') -> Free f a -> Free f' a
permute f = fold Pure (Op . f)

hErr :: Functor f' => Handler Err a f' (Either String a)
hErr = Handler
  { ret = pure . Right
  , hdlr = \x -> case x of Err s -> pure (Left s) }

data Handler_ f a p f' b
  = Handler_ { ret_  :: a -> (p -> Free f' b)
             , hdlr_ :: f (p -> Free f' b) -> (p -> Free f' b) }

handle_ :: (Functor f, Functor f')
        => Handler_ f a p f' b -> Free (f + f') a
        -> p -> Free f' b
handle_ h = fold
  (ret_ h)
  (\x -> case x of
     L l -> hdlr_ h l
     R r -> \p -> Op (fmap (\m -> m p) r))

un :: Free End a -> a
un (Pure x) = x
un (Op f) = case f of
