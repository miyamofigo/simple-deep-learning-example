import Numeric.LinearAlgebra

op::(R, R) -> R -> R -> R -> Int 
op ws b x1 x2 = case (sumElements (x * w) + b) > 0 of
    True -> 1
    _    -> 0
 where x = vector [x1, x2]
       w = vector [fst ws, snd ws]

pAnd  x1 x2 = op (0.5, 0.5) (-0.7) x1 x2
pNand x1 x2 = op (-0.5, -0.5) 0.7 x1 x2
pOr   x1 x2 = op (0.5, 0.5) (-0.2) x1 x2

pXor  x1 x2 = pAnd (fromIntegral s1) (fromIntegral s2)
 where s1 = pNand x1 x2
       s2 = pXor x1 x2  
