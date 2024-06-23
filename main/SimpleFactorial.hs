module SimpleFactorial where
import Prelude1
import EffectStrOut
import EffectState
import EFix (efixEither)

fix :: (a -> a) -> a
fix f = let x = f x in x

factorial :: (Int -> Int) -> Int -> Int
factorial f n
    | n <= 1 = 1
    | otherwise = n * (f (n - 1))

fact :: Int -> Int
fact = fix factorial

-- With algebraic effects (regular signature)
exampleFact :: (Int -> Free (StrOut + End) Int) -> Int -> Free (StrOut + End) Int
exampleFact f n =    do
                        if n <= 1 then
                            Pure n
                        else do
                            next <- f (n - 1)
                            let res = n * next
                            strout ("Curr: " ++ show res)
                            Pure res

exampleFactEffectless :: (Int -> Free End Int) -> Int -> Free End Int
exampleFactEffectless f n =  do
                                if n <= 1 then
                                    Pure n
                                else do
                                    next <- f (n - 1)
                                    return (n * next)

-- With algebraic effects (either signature)
factorialEither :: (Int, Int) -> Free End (Either (Int, Int) Int)
factorialEither (n, cnt)
    | n <= 1 = Pure (Right cnt)
    | otherwise = Pure (Left (n - 1, n * cnt))

factEither :: Int -> Int
factEither n = un ((efixEither factorialEither) (n, 1))
