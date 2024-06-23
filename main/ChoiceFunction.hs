module ChoiceFunction where
import System.Random (StdGen)
import Prelude1
import EffectChoice
import EffectRandomGenerator
import EFix (efixEither)

-- Note the extra effect "RandomGenerator" - this exists due to our implementation of the choice effect
-- Essentially, it keeps track of and continuously creates new generators to ensure that different
-- random values are generated every time
-- See also `EffectChoice.hs`
choices :: Int -> Free (Choice + RandomGenerator + End) (Either Int Int)
choices k = do
                                b <- choice
                                if b then do
                                    return (Right k)
                                else do
                                    return (Left (k + 1))

choicesResult :: StdGen -> (Int, [String])
choicesResult initGen = un (handle_ hStrOut (handle_ hRandomGenerator (handle hChoiceRandom (efixEither choices 0)) initGen) [])

choicesResultPrintable :: StdGen -> String
choicesResultPrintable initGen = let p = choicesResult initGen in "(" ++ show (fst p) ++ ", " ++ show (snd p) ++ ")"
