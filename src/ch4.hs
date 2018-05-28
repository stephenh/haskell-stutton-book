module Ch4 () where

halve :: [a] -> ([a], [a])
halve x = (take (div (length x) 2) x, drop (div (length x) 2) x)

-- with head & tail
third1 :: [a] -> a
third1 x = head (tail (tail x))

-- this !! operator
third2 :: [a] -> a
third2 x = x !! 2

-- with pattern matching
third3 :: [a] -> a
third3 (_:_:x:_) = x

-- with if expression
safetail1 :: [a] -> [a]
safetail1 x = if null x then [] else tail x

-- with guard expression
safetail2 :: [a] -> [a]
safetail2 x | null x    = []
            | otherwise = tail x

-- with pattern matching
safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 (x:xs) = xs

-- || defined explicitly
o1 :: Bool -> Bool -> Bool
True `o1` False  = True
False `o1` True  = True
True `o1` True   = True
False `o1` False = False

-- || defined explicitly
o2 :: Bool -> Bool -> Bool
False `o2` False = False
_ `o2` _         = True

-- || defined explicitly
o3 :: Bool -> Bool -> Bool
True `o3` _  = True
False `o3` b = b

-- && defined with if expressions
a1 :: Bool -> Bool -> Bool
a1 a b = if a then (if b then True else False) else False

-- && defined with if expressions
a2 :: Bool -> Bool -> Bool
a2 a b = if a then b else False

mult :: Int -> Int -> Int -> Int
mult x = (\y -> (\z -> x * y * y))

luhnDouble :: Int -> Int
luhnDouble x | x * 2 > 9 = x * 2 - 9
             | otherwise = x * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = (luhnDouble a + b + luhnDouble c + d) `mod` 10 == 0

