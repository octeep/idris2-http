module Data.Compress.Utils.Misc

import Data.Fin
import Data.Vect
import Data.List
import Data.Bits
import Data.List1
import Data.SnocList

export
take_last : Nat -> SnocList a -> Maybe (List a)
take_last = loop Lin where
  loop : SnocList a -> Nat -> SnocList a -> Maybe (List a)
  loop acc Z _ = Just (asList acc)
  loop acc (S n) (init :< last) = loop (acc :< last) n init
  loop acc _ Lin = Nothing

export
stream_concat : Stream (List a) -> Stream a
stream_concat ([] :: ys) = stream_concat ys
stream_concat ((x :: xs) :: ys) = x :: stream_concat (xs :: ys)

export
index_may : Nat -> List a -> Maybe a
index_may Z (x :: xs) = Just x
index_may (S n) (x :: xs) = index_may n xs
index_may _ [] = Nothing

export
count : Ord a => List a -> List (a, Nat)
count = map (\xs => (head xs, length xs)) . group . sort
