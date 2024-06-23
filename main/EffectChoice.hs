{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
module EffectChoice where
import System.Random

import Prelude1
import EffectRandomGenerator

data Choice k
  = Choose (Bool -> k)
  deriving Functor

choice :: Choice < f => Free f Bool
choice = Op (inj (Choose Pure))

hChoiceForceTrue :: Functor f' => Handler Choice a f' a
hChoiceForceTrue = Handler
  { ret = pure
  , hdlr = \x -> case x of Choose k -> k True }

hChoiceForceFalse :: Functor f' => Handler Choice a f' a
hChoiceForceFalse = Handler
  { ret = pure
  , hdlr = \x -> case x of Choose k -> k False }

hChoiceRandom :: (Functor f', RandomGenerator < f') => Handler Choice a f' a
hChoiceRandom = Handler
  {
    ret = pure
  , hdlr = \x -> case x of Choose k ->  do
                                          gen <- getGenerator
                                          let (b, ng) = uniform gen :: (Bool, StdGen)
                                          putGenerator ng
                                          k b
  }