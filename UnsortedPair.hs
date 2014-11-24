module UnsortedPair (UnsortedPair(), upair, pair) where


data UnsortedPair a = UnsortedPair a a

upair :: (Ord a) => a -> a -> UnsortedPair a
upair a b | a<b       = UnsortedPair a b
          | otherwise = UnsortedPair b a

instance (Eq a) => Eq (UnsortedPair a) where
  (UnsortedPair a b) == (UnsortedPair c d)  =  a == c && b == d

instance (Ord a) => Ord (UnsortedPair a) where
  (UnsortedPair a b) <= (UnsortedPair c d) = a<c || (a==c && b<=d)

pair :: UnsortedPair a -> (a, a)
pair (UnsortedPair a b) = (a, b)
