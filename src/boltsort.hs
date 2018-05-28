
newtype Nut = Nut Int deriving Show
newtype Bolt = Bolt Int deriving Show

compareNutBolt :: Nut -> Bolt -> Int
compareNutBolt (Nut i) (Bolt j) = i - j

-- given a compare functions, breaks [a] into smaller/equal/bigger
partition :: (a -> Int) -> [a] -> ([a], [a], [a])
partition f [] = ([], [], [])
partition f (a:as) = (includeIf (0>) ++ ls, includeIf (0==) ++ es, includeIf (0<) ++ gs)
  where v = f a
        (ls, es, gs) = partition f as
        includeIf op = if (op v) then [a] else []

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

boltsort2 :: ([Nut], [Bolt]) -> ([Nut], [Bolt])
boltsort2 ([], []) = ([], [])
boltsort2 ((n:ns), bs) = (ssn ++ [n] ++ sln, ssb ++ [b] ++ slb) where
  (sb, [b], lb) = partition (\b -> negate (compareNutBolt n b)) bs
  (sn, _, ln) = partition (\n -> compareNutBolt n b) ns
  (ssn, ssb) = boltsort2 (sn, sb)
  (sln, slb) = boltsort2 (ln, lb)

nuts = [Nut 1, Nut 3, Nut 2, Nut 4]
bolts = [Bolt 4, Bolt 2, Bolt 3, Bolt 1]
-- boltsort (nuts, bolts)

