
newtype Nut = Nut Int deriving Show
newtype Bolt = Bolt Int deriving Show

-- tries to fit a nut & bolt, returns <0 if nut is smaller, >0 if nut is bigger, 0 if it fits
compareNutBolt :: Nut -> Bolt -> Int
compareNutBolt (Nut i) (Bolt j) = i - j

-- given a compare functions, breaks [a] into smaller/equal/bigger
partition :: (a -> Int) -> [a] -> ([a], [a], [a])
partition f [] = ([], [], [])
partition f (a:as) | c < 0     = (a:ls, es, gs)
                   | c == 0    = (ls, a:es, gs)
                   | otherwise = (ls, es, a:gs)
  where c = f a; (ls, es, gs) = partition f as

-- first take/more verbose sorting of nuts/bolts using only compareNutBolt
boltsort :: ([Nut], [Bolt]) -> ([Nut], [Bolt])
boltsort ([], []) = ([], [])
boltsort ((n:ns), bs) = (ssn ++ [n] ++ sln, ssb ++ [b] ++ slb) where
  sb = filter (\b -> compareNutBolt n b > 0) bs
  lb = filter (\b -> compareNutBolt n b < 0) bs
  b = head (filter (\b -> compareNutBolt n b == 0) bs)
  sn = filter (\n -> compareNutBolt n b < 0) ns
  ln = filter (\n -> compareNutBolt n b > 0) ns
  (ssn, ssb) = boltsort (sn, sb)
  (sln, slb) = boltsort (ln, lb)

-- second take with cute partition method
boltsort2 :: ([Nut], [Bolt]) -> ([Nut], [Bolt])
boltsort2 ([], []) = ([], [])
boltsort2 ((n:ns), bs) = (sn' ++ [n] ++ ln', sb' ++ [b] ++ lb') where
  (sb, [b], lb) = partition (\b -> negate (compareNutBolt n b)) bs
  (sn, _, ln) = partition (\n -> compareNutBolt n b) ns
  (sn', sb') = boltsort2 (sn, sb)
  (ln', lb') = boltsort2 (ln, lb)

nuts = [Nut 1, Nut 3, Nut 2, Nut 4]
bolts = [Bolt 4, Bolt 2, Bolt 3, Bolt 1]
-- boltsort (nuts, bolts)

