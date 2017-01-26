import qualified Data.HashMap as M 
import Numeric.LinearAlgebra
import Graphics.Gnuplot.Simple

step::Matrix R -> Matrix R 
step = cmap (\x -> if x > 0 then 1.0 else 0)

sigmoid::Matrix R -> Matrix R 
sigmoid = cmap (\x -> 1.0 / (1.0 + exp (-x)))

xcoords::R -> R -> [R]
xcoords mini maxi= linearScale 1000 (mini, maxi)

toPoints::(Matrix R -> Matrix R) -> [R] -> [(R, R)]
toPoints f xs = zip xs (convert xs)
    where convert = head . toLists . f . matrix (length xs) 

plot f (mini, maxi) = plotList [YRange (mini, maxi)] 
    (toPoints f (xcoords (-5.0) 5.0))

reLU::Matrix R -> Matrix R 
reLU = cmap (\x -> max 0 x)

identity_function = id  

_W1 = "W1"
_W2 = "W2"
_W3 = "W3"

_b1 = "b1"
_b2 = "b2"
_b3 = "b3"

initNetwork = M.fromList 
    [(_W1, matrix 3 [0.1, 0.3, 0.5, 0.2, 0.4, 0.6]),
     (_b1, matrix 3 [0.1, 0.2, 0.3]),
     (_W2, matrix 2 [0.1, 0.4, 0.2, 0.5, 0.3, 0.6]),
     (_b2, matrix 2 [0.1, 0.2]),
     (_W3, matrix 2 [0.1, 0.3, 0.2, 0.4]),
     (_b3, matrix 2 [0.1, 0.2])]

convert x y z = sigmoid $ z <> x + y 

forward network x = let
    w1 = network M.! _W1      
    w2 = network M.! _W2      
    w3 = network M.! _W3
    b1 = network M.! _b1      
    b2 = network M.! _b2      
    b3 = network M.! _b3
    signal     = convert w2 b2 . convert w1 b1  
    activate x = id $ x <> w3 + b3
 in
    activate $ signal x 

softmax::Matrix R -> Matrix R
softmax a = cmap (\x -> x/sumExpA) expA  
 where
    c       = maxElement a
    expA    = cmap exp $ cmap (\x -> x-c) a
    sumExpA = sum $ head $ toLists expA 
