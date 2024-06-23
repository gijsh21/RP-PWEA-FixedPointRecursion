module CircuitsExample where
import Prelude1
import EffectStrOut
import EFix (efixEither)

type Signal = [Bool]

data CircuitComponent k
        = CircAnd Signal Signal (Signal -> k)
        | CircXor Signal Signal (Signal -> k)
        | CircInv Signal (Signal -> k)
        | CircDelay Bool Signal (Signal -> k)
        deriving Functor

circ_and :: CircuitComponent < f => Signal -> Signal -> Free f Signal
circ_and x y = Op (inj (CircAnd x y Pure))

circ_xor :: CircuitComponent < f => Signal -> Signal -> Free f Signal
circ_xor x y = Op (inj (CircXor x y Pure))

circ_inv :: CircuitComponent < f => Signal -> Free f Signal
circ_inv x = Op (inj (CircsInv x Pure))

circ_delay :: CircuitComponent < f => Bool -> Signal -> Free f Signal
circ_delay n v x = Op (inj (CircsDelay v x Pure))

hCircuitSimulate :: Functor f' => Handler CircuitComponent a f' a
hCircuitSimulate = Handler
        {
                ret = pure,
                hdlr = \x -> case x of
                                CircAnd x y k -> k (zipWith (&&) x y)
                                CircXor x y k -> k (zipWith (/=) x y)
                                CircInv x k -> k (map not x)
                                CircDelay v x k -> k (v : x)
        }

halfAdder :: Signal -> Signal -> Free (CircuitComponent + End) (Signal, Signal)
halfAdder x y = do
                        res <- circ_xor x y
                        carry <- circ_and x y
                        return (res, carry)

halfAdderExample :: (Signal, Signal)
halfAdderExample = un (handle hCircuitSimulate (halfAdder [True, True] [False, True]))
-- ([True, False], [False, True])

toggleFixable :: (Signal, Signal) -> Free (CircuitComponent + End) (Either (Signal, Signal) a)
toggleFixable (inp, out) =   do
                                inp <- circ_inv out
                                out <- circ_delay False inp
                                return (Left (inp, out))

toggle :: Signal
toggle = un ((handle hCircuitSimulate (efixEither toggleFixable ([], []))) >>= (\(inp, out) -> return out))
-- [False, True, False, True, ...
