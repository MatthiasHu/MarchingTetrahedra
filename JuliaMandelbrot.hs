module JuliaMandelbrot
  ( escapeTime
  , escapes
  ) where

import Data.Complex


escapes :: (RealFloat a) => Int -> a -> a -> a -> a -> Bool
escapes tmax a b c d = escapeTime tmax a b c d >= tmax

escapeTime :: (RealFloat a) => Int -> a -> a -> a -> a -> Int
escapeTime tmax a b c d = escapeTime' tmax (a :+ b) (c :+ d)

escapeTime' :: (RealFloat a) => Int -> Complex a -> Complex a -> Int
escapeTime' tmax z0 c =
    length . take tmax
  . takeWhile ((<=magBound) . magnitude)
  $ iterate f z0
  where
    f z = z^2 + c
    magBound = 10
