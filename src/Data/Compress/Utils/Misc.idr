module Data.Compress.Utils.Misc

import Data.Fin
import Data.Vect
import Data.List
import Data.Bits
import Data.List1
import Data.SnocList
import Data.Compress.Utils.ParseT

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
get_bit : Num n => Parser Bitstream (SimpleError String) n
get_bit = map (\b => if b then 1 else 0) token

fin_range : (n : Nat) -> List (Fin n)
fin_range _ = toList Fin.range

export
get_huff, get_bits : Fin 32 -> Parser Bitstream (SimpleError String) Bits32
get_bits n = do
  let n' = finToNat n
  bits <- count n' token
  pure $ foldl (\a,(i,b) => if b then setBit a i else a) 0 $ zip (fin_range 32) (toList bits)

get_huff n = do
  let n' = finToNat n
  bits <- count n' token
  pure $ foldl (\a,(i,b) => if b then setBit a i else a) 0 $ zip (fin_range 32) (reverse $ toList bits)

export
count : Ord a => List a -> List (a, Nat)
count = map (\xs => (head xs, length xs)) . group . sort
