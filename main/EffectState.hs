{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
module EffectState where
import Prelude1

data State s k
  = Put s k
  | Get (s -> k)
  deriving Functor

get :: State s < f => Free f s
get = Op (inj (Get Pure))

put  :: State s < f => s -> Free f ()
put s = Op (inj (Put s (Pure ())))

err :: Err < f => String -> Free f a
err msg = Op (inj (Err msg))

hState :: Functor g => Handler_ (State s) a s g (a, s)
hState = Handler_
  { ret_ = \x s -> pure (x, s)
  , hdlr_ = \x s -> case x of
      Put s' k -> k s'
      Get k -> k s s }

--incerr' :: Free (Err + State Int + End) a
--incerr' = do (s :: Int) <- get; put (s + 1); err "foo"

--res :: (Either String a, Int)
--res = un (handle_ hState (handle hErr incerr') 0)
