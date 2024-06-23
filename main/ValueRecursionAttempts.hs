module ValueRecursionAttempts where
import Prelude1
import EffectStrOut
import EFix (efixEither)


-- Example with ChIn
-- Note that the return value is wrapped in IO because of our implementation of the ChIn effect
freeGetChar :: IO String -> Free (ChIn + End) (IO String)
freeGetChar cs =        do
                                c <- chin
                                return (do
                                                p <- c
                                                ps <- cs
                                                return (p:ps)
                                        )


-- Test function for the dummy handler
hStrOutDummy :: Functor f' => Handler StrOut a f' a
hStrOutDummy = Handler {
        ret = pure,
        hdlr = \x -> case x of Out _ k -> k
}

dummyTest :: Int -> Free (StrOut + End) (Either Int Int)
dummyTest n =      do
                                        strout ("Argument was " ++ (show n))
                                        if n <= 0 then do
                                                return $ Right 0
                                        else do
                                                return $ Left (n - 1)

dummyFixStrOut f h1 h2 x = (handle_ h1 (f x) []) >>=      (\(m, strs) ->
                                                        case m of
                                                                Right r -> return (r, strs)
                                                                Left l -> do
                                                                                res <- (handle h2 (efixEither f l))
                                                                                return (res, strs)
                                                )

dummyResult = un (dummyFixStrOut dummyTest hStrOut hStrOutDummy 5)
