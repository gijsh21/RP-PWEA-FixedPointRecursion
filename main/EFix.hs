{-# LANGUAGE TypeOperators #-}
module EFix where
import Prelude1
import EffectStrOut

efixEither :: Functor f => (a -> Free f (Either a b)) -> a -> Free f b
efix f x = do
  y <- f x
  case y of
    Left l -> efixEither f l
    Right r -> Pure r

efixRegular :: ((a -> Free f a) -> a -> Free f a) -> a -> Free f a
efixRegular f n = (let x = f x in x) n

testfune :: Int -> Free (StrOut + End) (Either Int Int)
testfune n =  do
                strout ("Saw " ++ show n)
                if n < 0 then
                  return (Left (n + 1))
                else if n == 0 then
                  return (Right 0)
                else
                  return (Left (n - 1))

testfuneNoDo :: Int -> Free (StrOut + End) (Either Int Int)
testfuneNoDo n = Op (L (Out ("Saw " ++ show n)
                      (if n < 0 then
                        Pure $ Left (n + 1)
                      else if n == 0 then
                        Pure $ Right 0
                      else
                        Pure $ Left (n - 1)
                      )
                  ))

testfuneRegular :: (Int -> Free (StrOut + End) Int) -> Int -> Free (StrOut + End) Int
testfuneRegular f n = do
                  strout ("Saw " ++ show n)
                  if n < 0 then
                    f (n + 1)
                  else if n == 0 then
                    Pure 0
                  else
                    f (n - 1)

efixEitherres :: (Int, [String])
efixEitherres = un (handle_ hStrOut (efixEither testfune 5) [])

efixRegularRes :: (Int, [String])
efixRegularRes = un (handle_ hStrOut (efixRegular testfune3 5) [])
