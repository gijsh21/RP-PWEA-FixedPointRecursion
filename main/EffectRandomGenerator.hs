{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
module EffectRandomGenerator where
import System.Random (StdGen)
import Prelude1

data RandomGenerator k
  = PutGenerator StdGen k
  | GetGenerator (StdGen -> k)
  deriving Functor

putGenerator :: RandomGenerator < f => StdGen -> Free f ()
putGenerator gen = Op (inj (PutGenerator gen (Pure ())))

getGenerator :: RandomGenerator < f => Free f StdGen
getGenerator = Op (inj (GetGenerator Pure))

hRandomGenerator :: Functor g => Handler_ RandomGenerator a StdGen g a
hRandomGenerator = Handler_
  {
    ret_ = \x _ -> pure x,
    hdlr_ = \x s -> case x of
      PutGenerator gen k -> k gen
      GetGenerator k -> k s s
  }
