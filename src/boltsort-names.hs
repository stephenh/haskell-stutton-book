
-- This is exactly the same code as `boltsort.hs` but with
-- "non-Haskell" style variable names. E.g. boring, enterprise Java,
-- non-abbreviation, wow I can actually read them names.

newtype Nut = Nut Int deriving Show
newtype Bolt = Bolt Int deriving Show

-- The problem is to sort two buckets of nuts and bolts, given
-- only a comparison function of nut-to-bolt, e.g. you can't
-- compare nuts against themselves or bolts against themselves.
-- Also, as a simplification, there is only a single nut=bolt
-- match for a given size, e.g. all nuts are uniquely sized and
-- all bolts are uniquely sized, but all nuts/bolts have a single
-- match.

-- tries to fit a nut & bolt, returns <0 if nut is smaller, >0 if nut is bigger, 0 if it fits
compareNutBolt :: Nut -> Bolt -> Int
compareNutBolt (Nut i) (Bolt j) = i - j

-- given a compare function, breaks [a] into a tuple of ([smaller], [equal], [bigger])
partition :: (a -> Int) -> [a] -> ([a], [a], [a])
partition compareFn [] = ([], [], [])
partition compareFn (a:as)
    | compare < 0   = (a:lesser, equal, greater)
    | compare == 0  = (lesser, a:equal, greater)
    | otherwise     = (lesser, equal, a:greater)
  where
    compare = compareFn a
    (lesser, equal, greater) = partition compareFn as

-- first take/more verbose sorting of nuts/bolts using only compareNutBolt.
-- this is basically a two-pass quicksort, which:
-- 1) picks a nut as a pivot
-- 2) partitions the bolts using the nut-pivot
-- 3) using the nut pivot to find the matching bolt pivot
-- 4) partitions the nuts using the bolt-pivot
-- 5) recurses/combines
boltsort :: ([Nut], [Bolt]) -> ([Nut], [Bolt])
boltsort ([], []) = ([], [])
boltsort ((pivotNut:nuts), bolts) = (sortedSmallerNuts ++ [pivotNut] ++ sortedLargerNuts, sortedSmallerBolts ++ [pivotBolt] ++ sortedLargerBolts)
  where
    smallerBolts = filter (\b -> compareNutBolt pivotNut b > 0) bolts
    largerBolts = filter (\b -> compareNutBolt pivotNut b < 0) bolts
    pivotBolt = head (filter (\b -> compareNutBolt pivotNut b == 0) bolts)
    smallerNuts = filter (\n -> compareNutBolt n pivotBolt < 0) nuts
    largerNuts = filter (\n -> compareNutBolt n pivotBolt > 0) nuts
    (sortedSmallerNuts, sortedSmallerBolts) = boltsort (smallerNuts, smallerBolts)
    (sortedLargerNuts, sortedLargerBolts) = boltsort (largerNuts, largerBolts)

-- second take with cute partition method
boltsort2 :: ([Nut], [Bolt]) -> ([Nut], [Bolt])
boltsort2 ([], []) = ([], [])
boltsort2 ((pivotNut:nuts), bolts) =
    (sortedSmallerNuts ++ [pivotNut] ++ sortedLargerNuts, sortedSmallerBolts ++ [pivotBolt] ++ sortedLargerBolts)
  where
    (smallerBolts, [pivotBolt], largerBolts) = partition (\b -> negate (compareNutBolt pivotNut b)) bolts
    (smallerNuts, _, largerNuts) = partition (\n -> compareNutBolt n pivotBolt) nuts
    (sortedSmallerNuts, sortedSmallerBolts) = boltsort2 (smallerNuts, smallerBolts)
    (sortedLargerNuts, sortedLargerBolts) = boltsort2 (largerNuts, largerBolts)

nuts = [Nut 1, Nut 3, Nut 2, Nut 4]
bolts = [Bolt 4, Bolt 2, Bolt 3, Bolt 1]
-- boltsort (nuts, bolts)

