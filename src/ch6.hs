
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

exp2 :: Int -> Int -> Int
exp2 n 0 = 1
exp2 n x = n * exp2 n (x-1)

euclid :: Int -> Int -> Int
euclid x y | x == y = x
           | x < y  = euclid x (y - x)
           | otherwise = euclid (x - y) y

and2 :: [Bool] -> Bool
and2 [] = False
and2 [b] = b
and2 (False:xs) = False
and2 (True:xs) = and2(xs)

concat2 :: [[a]] -> [a]
concat2 [xs] = xs
concat2 (xs:xss) = xs ++ concat2(xss)

replicate2 :: Int -> a -> [a]
replicate2 0 _ = []
replicate2 n x = x : replicate2 (n-1) x

nth :: [a] -> Int -> a
nth (x:_) 0 = x
nth (x:xs) n = nth xs (n-1)

elem2 :: Eq a => a -> [a] -> Bool
elem2 x [] = False
elem2 x (y:ys) = if x == y then True else elem2 x ys

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = if x < y then x : merge xs (y:ys) else y : merge (x:xs) ys

halve :: [a] -> ([a], [a])
halve x = (take n x, drop n x) where n = length x `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort left) (msort right) where (left, right) = halve(xs)

