{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
module EffectStrOut where
import Prelude1
import System.IO.Unsafe (unsafePerformIO)

data StrOut k = Out String k
  deriving Functor

strout :: StrOut < f => String -> Free f ()
strout msg = Op (inj (Out msg (Pure ())))

hStrOut :: Functor g => Handler_ StrOut a [String] g (a, [String])
hStrOut = Handler_
  { ret_ = \x ss -> pure (x, ss)
  , hdlr_ = \x ss -> case x of
      Out msg k -> k (msg : ss) }

strOutExtract :: StrOut k -> k
strOutExtract (Out _ k) = k 

data ChIn k = In (IO Char -> k)
  deriving Functor

chin :: ChIn < f => Free f (IO Char)
chin = Op (inj (In Pure))

hChIn :: Functor f' => Handler ChIn a f' a
hChIn = Handler
  { ret = pure
  , hdlr = \x -> case x of In k -> k getChar }
