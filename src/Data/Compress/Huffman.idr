module Data.Compress.Huffman

import Data.Fin
import Data.Vect
import Data.List
import Data.List1
import Data.Bits
import Data.Stream
import Data.Compress.Utils.Misc
import Data.Compress.Utils.Parser

data Tree : Type -> Type where
  Node : Tree a -> Tree a -> Tree a
  Leaf : a -> Tree a

mk : List Bool -> a -> Tree (Maybe a)
mk [] k = Leaf (Just k)
mk (False :: dirs) k = Node (mk dirs k) (Leaf Nothing)
mk (True :: dirs) k = Node (Leaf Nothing) (mk dirs k)

insert : List Bool -> a -> Tree (Maybe a) -> Maybe (Tree (Maybe a))
insert [] k (Leaf Nothing) = pure $ Leaf (Just k)
insert [] k (Leaf (Just _)) = Nothing
insert [] k (Node _ _) = Nothing
insert (False :: xs) k (Node left right) = pure $ Node !(insert xs k left) right
insert (True :: xs) k (Node left right) = pure $ Node left !(insert xs k right)
insert (False :: xs) k (Leaf Nothing) = pure $ Node !(insert xs k (Leaf Nothing)) (Leaf Nothing)
insert (True :: xs) k (Leaf Nothing) = pure $ Node (Leaf Nothing) !(insert xs k (Leaf Nothing))
insert (_ :: _) k (Leaf (Just _)) = Nothing

lookup_count : Eq a => List (a, Nat) -> a -> Nat
lookup_count list key =
  case lookup key list of
    Just x => x
    Nothing => Z

max' : Ord a => List1 a -> a
max' (x ::: xs) = foldl max x xs

smallest_codes : (Bits32 -> Nat) -> (n : Nat) -> Vect (S n) Bits32
smallest_codes bl_count max_code_length =
  take (S max_code_length) $ map fst $ iterate go (0,0)
  where
    go : (Bits32, Bits32) -> (Bits32, Bits32)
    go (prev_code, prev_index) = (shiftL (prev_code + cast (bl_count prev_index)) 1, prev_index + 1)

make_tree_from_length : {n : Nat} -> List Bits32 -> Vect (S n) Bits32 -> List Bits32 -> Maybe (List Bits32)
make_tree_from_length acc next_code [] = Just $ reverse acc
make_tree_from_length acc next_code (x :: xs) = do
  x' <- natToFin (cast x) (S n)
  let v = index x' next_code
  make_tree_from_length (v :: acc) (updateAt x' (+1) next_code) xs

decompose_bits32 : Bits32 -> Fin 32 -> List Bool
decompose_bits32 i FZ = [ testBit i FZ ]
decompose_bits32 i (FS n) = testBit i (FS n) :: decompose_bits32 i (weaken n)

decompose_l_i : Bits32 -> Bits32 -> Maybe (List Bool)
decompose_l_i l i = decompose_bits32 i <$> natToFin (cast (l-1)) 32 

public export
record HuffmanTree where
  constructor MkTree
  parse_literals : Parser Bitstream (SimpleError String) Bits32
  parse_distance : Parser Bitstream (SimpleError String) Bits32

export
default_tree : HuffmanTree
default_tree = MkTree first (get_huff 5) where
  first : Parser Bitstream (SimpleError String) Bits32
  first = do
    x <- get_huff 7
    if x < 24 then pure (x + 256) else do
      x <- map ((shiftL x 1) .|.) get_bit
      if x < 192 then pure (x - 48)
        else if x < 200 then pure (x + 88)
        else map (\y => ((shiftL x 1) .|. y) - 256) get_bit

tree_to_parser : Tree (Maybe a) -> Parser Bitstream (SimpleError String) a
tree_to_parser (Leaf Nothing) = fail $ msg "no value at leaf"
tree_to_parser (Leaf (Just v)) = pure v
tree_to_parser (Node false true) = do
  b <- token
  if b then tree_to_parser true else tree_to_parser false

export
make_tree : Ord a => List (a, Bits32) -> Nat -> Maybe (Parser Bitstream (SimpleError String) a)
make_tree elem_code_length max_n_code = do
  let elem_code_length = filter (\(a,b) => b > 0) $ sortBy (\a,b => compare (fst a) (fst b)) elem_code_length
  let code_length = map snd elem_code_length

  guard ((length code_length) <= max_n_code)
  let code_length_count = count code_length
  let bl_count = lookup_count code_length_count
  code_length_count1 <- fromList code_length_count

  let max_code_length = cast $ max' $ map fst code_length_count1
  let next_code = smallest_codes bl_count max_code_length
  
  tree <- make_tree_from_length [] next_code code_length
  elem_code <- traverse (\(i,(v,l)) => (,v) <$> decompose_l_i l i) $ zip tree elem_code_length
  (elem_code_head ::: elem_code_tail) <- fromList elem_code
  
  b_tree <- foldlM (\t,(p,v) => insert p v t) (uncurry mk elem_code_head) elem_code_tail

  pure $ tree_to_parser b_tree
