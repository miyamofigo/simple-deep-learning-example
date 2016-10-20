import Numeric.LinearAlgebra

op::(Double, Double) -> Double -> Double -> Double -> Int 
op ws b x1 x2 = case (sumElements (x * w) + b) > 0 of
    True -> 1
    _    -> 0
 where x = vector [x1, x2]
       w = vector [fst ws, snd ws]

and  x1 x2 = op (0.5, 0.5) (-0.7) x1 x2
nand x1 x2 = op (-0.5, -0.5) 0.7 x1 x2
or   x1 x2 = op (0.5, 0.5) (-0.2) x1 x2

xor  x1 x2 = Main.and (fromIntegral s1) (fromIntegral s2)
 where s1 = Main.nand x1 x2
       s2 = Main.or x1 x2  
