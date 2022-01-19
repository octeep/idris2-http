module Data.Compress.GZip

import Data.Compress.Utils.Bitstream
import Data.Compress.Utils.Misc
import Data.Vect
import Data.Bits
import Data.List
import Data.SnocList
import Data.Stream
import Control.Monad.Error.Either
import Utils.Bytes
import Utils.Streaming

import Debug.Trace

import public Data.Compress.Deflate

export
parse_gzip_header : Monad m => BParser (Either e String) r m Bool
parse_gzip_header = do
  [0x1f, 0x8b] <- ntimes 2 next_byte
  | [0x00, 0x00] => pure False -- eof
  | x => fail $ Right "gzip magic number expected, got \{show x}"
  0x08 <- next_byte
  | x => fail $ Right "deflate method magic number expected, got \{show x}"
  flag <- next_byte
  mtime <- le_nat 4
  xfl <- next_byte
  os <- next_byte

  fextra <- ifA (testBit flag 2) $ do
    xlen <- le_nat 2
    toList <$> ntimes xlen next_byte

  fname <- ifA (testBit flag 3) (skip_till (== 0))

  fcomment <- ifA (testBit flag 4) (skip_till (== 0))

  fhcrc <- ifA (testBit flag 1) (le_nat 2)

  pure True

export
parse_gzip_footer : Monad m => BParser e' r m (Bits32, Bits32)
parse_gzip_footer = do
  crc32 <- cast <$> le_nat 4
  isize <- cast <$> le_nat 4
  pure (crc32, isize)

export
decompress_gzip : Monad m => Stream (Of Bits8) m (Either e r) -> Stream (Of Bits8) m (Either (Either (Either e String) r) ())
decompress_gzip stream =
  map to_either
  $ (parse_gzip_header *> parse_deflate Lin $> ()).run_parser
  $ from_bytestream
  $ map (mapFst Left) stream 
