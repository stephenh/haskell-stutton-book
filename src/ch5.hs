
module Ch5 () where

sumHundredSquares :: Int
sumHundredSquares = sum [ x * x | x <- [1..100]]

grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0..m], y <- [0..n]]

square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]

replicate2 :: Int -> a -> [a]
replicate2 n v = [v | _ <- [1..n]]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x*x + y*y == z*z]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (filter (/= x) (factors x)) == x]

scalarprod :: [Int] -> [Int] -> Int
scalarprod xs ys = sum [x*y | (x, y) <- zip xs ys]
