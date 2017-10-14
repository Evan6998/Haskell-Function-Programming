module Newton_Raphson where
import Test.QuickCheck

squareroot2 :: Float -> Integer -> Float
squareroot2 num 0 = num
squareroot2 num epochs = let num' = (num + 2/num)/2
                         in squareroot2 num' (epochs-1)

squareroot :: Float -> Float -> Integer -> Float
squareroot num start 0 = start
squareroot num start epochs = let start' = (start + num/start)/2
                              in squareroot num start' (epochs-1)

sqrtSeq :: Float -> Float -> [Float]
sqrtSeq num start = [squareroot num start x | x <- [1..]]

test :: Num a => (a -> Bool) -> [a] -> a  
test p (x:y:xs)
    | p (abs (x-y)) = y
    | otherwise = test p (y:xs)

squareroot' :: Float -> Float -> Float -> Float
squareroot' num start epsilon = test (<epsilon) (sqrtSeq num start)
