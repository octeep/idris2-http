module Data.Compress.Utils.Bitstream

import Data.Vect
import Data.Fin
import Data.Bits
import Data.List
import Utils.Bytes
import Utils.Streaming
import Control.Monad.Error.Either
import Control.Monad.Trans

export
record Bitstream (m : Type -> Type) (e : Type) (r : Type) where
  constructor MkBitstream
  offset : Fin 8
  current : Bits8
  bytestream : Stream (Of Bits8) m (Either e r)

export
data Result : (Type -> Type) -> Type -> Type -> Type -> Type where
  Fail : (Either e r) -> Result m e r a
  OK   : a -> (Bitstream m e r) -> Result m e r a

export
Functor (Result m e r) where
  map f (Fail err) = Fail err
  map f (OK r s)   = OK (f r) s

export
record BParser (e : Type) (r : Type) (m : Type -> Type) (a : Type) where
  constructor P
  runParser : Bitstream m e r -> Stream (Of Bits8) m (Result m e r a)

public export
Functor m => Functor (BParser e r m) where
  map f p = P $ \s => map (map f) (p.runParser s)

public export
Monad m => Applicative (BParser e r m) where
  pure x = P $ \s =>
    pure $ OK x s
  f <*> x = P $ \s =>
    case !(f.runParser s) of
      OK f' s' => map (map f') (x.runParser s')
      Fail err => pure $ Fail err

public export
Monad m => Monad (BParser e r m) where
  m >>= k = P $ \s => assert_total $
    (m.runParser s) >>= \case
      OK a s' => (k a).runParser s'
      Fail err => pure $ Fail err

public export
MonadTrans (BParser e r) where
  lift x = P $ \s => lift $ map (flip OK s) x

export
from_bytestream : Stream (Of Bits8) m (Either e r) -> Bitstream m e r
from_bytestream = MkBitstream 7 0

export
to_bytestream : Bitstream m e r -> Stream (Of Bits8) m (Either e r)
to_bytestream = bytestream

||| Get the next bit from the stream, or the return value if eos has been reached
export
next_bit : Monad m => BParser e r m Bool
next_bit = P $ \bitstream =>
  case strengthen (FS bitstream.offset) of
    Just i =>
      pure $ OK (testBit bitstream.current i) ({offset := i} bitstream)
    Nothing => do
      Right (byte, rest) <- lift $ next bitstream.bytestream
      | Left err => pure $ Fail err
      pure $ OK (testBit byte 0) (MkBitstream 0 byte rest)

||| Discard all bits until the next byte boundary, then get the next bit from the stream,
||| or the return value if eos has been reached
export
next_byte : Monad m => BParser e r m Bits8
next_byte = P $ \case
  (MkBitstream FZ current rest) =>
    pure $ OK current (from_bytestream rest)
  (MkBitstream (FS _)  _  rest) => do
    Right (byte, rest) <- lift $ next rest
    | Left err => pure $ Fail err
    pure $ OK byte (from_bytestream rest)

export
ntimes : Monad m => (n : Nat) -> BParser e r m a -> BParser e r m (Vect n a)
ntimes    Z  p = pure Vect.Nil
ntimes (S n) p = [| p :: (ntimes n p) |]

fin_range : (n : Nat) -> List (Fin n)
fin_range _ = toList Fin.range

le_bool_to_bits32 : List Bool -> Bits32
le_bool_to_bits32 = foldl (\a,(i,b) => if b then setBit a i else a) 0 . zip (fin_range 32)

be_bool_to_bits32 : List Bool -> Bits32
be_bool_to_bits32 = le_bool_to_bits32 . reverse

export
get_bits : Monad m => Fin 32 -> BParser e r m Bits32
get_bits n = le_bool_to_bits32 . toList <$> ntimes (finToNat n) next_bit

export
get_huff : Monad m => Fin 32 -> BParser e r m Bits32
get_huff n = be_bool_to_bits32 . toList <$> ntimes (finToNat n) next_bit

export
le_nat : Monad m => Nat -> BParser e r m Integer
le_nat n = le_to_integer <$> ntimes n next_byte
