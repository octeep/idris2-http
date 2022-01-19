module Data.Compress.GZip

import Data.Compress.Utils.Parser
import Data.Compress.Utils.Misc
import Data.Vect
import Data.Bits
import Data.List
import Data.SnocList
import Data.Stream
import Control.Monad.Error.Either
import Utils.Bytes

import public Data.Compress.Deflate

nul_term_string : (Semigroup e, Cons (Posed Bits8) i, Monoid i) => Parser i e String
nul_term_string = map ascii_to_string $ take_until (0 ==) p_get

public export
data CompressionFactor : Type where
  Poorest : CompressionFactor
  Strongest : CompressionFactor

||| It should be called file system but the RFC says it is operating system
public export
data OperatingSystem : Type where
  FAT : OperatingSystem
  Amiga : OperatingSystem
  VMS : OperatingSystem
  Unix : OperatingSystem
  VMCMS : OperatingSystem
  Atari : OperatingSystem
  HPFS : OperatingSystem
  Macintosh : OperatingSystem
  ZSystem : OperatingSystem
  CPM : OperatingSystem
  TOPS20 : OperatingSystem
  NTFS : OperatingSystem
  QDOS : OperatingSystem
  Acorn : OperatingSystem
  UnknownOS : Bits8 -> OperatingSystem

from_id : Bits8 -> OperatingSystem
from_id  0 = FAT
from_id  1 = Amiga
from_id  2 = VMS
from_id  3 = Unix
from_id  4 = VMCMS
from_id  5 = Atari
from_id  6 = HPFS
from_id  7 = Macintosh
from_id  8 = ZSystem
from_id  9 = CPM
from_id 10 = TOPS20
from_id 11 = NTFS
from_id 12 = QDOS
from_id 13 = Acorn
from_id x  = UnknownOS x

public export
record GZipHeader where
  constructor MkGZipHeader
  mtime : Bits64
  compression_factor : Maybe CompressionFactor
  os : OperatingSystem
  extra_fields : Maybe (List Bits8)
  file_name : Maybe String
  comment : Maybe String
  header_crc : Maybe (Vect 2 Bits8)

public export
record GZipFooter where
  constructor MkGZipFooter
  crc32 : Bits32
  isize : Bits32

||| Nothing if eof
export
parse_gzip_header : (Cons (Posed Bits8) i, Monoid i) => Parser i (SimpleError String) (Maybe GZipHeader)
parse_gzip_header = do
  [0x1f, 0x8b] <- count 2 p_get
  | [0x00, 0x00] => pure Nothing -- eof
  | x => fail $ msg "gzip magic number expected, got \{show x}"
  0x08 <- p_get
  | x => fail $ msg "deflate method magic number expected, got \{show x}"
  flag <- p_get
  mtime <- count 4 p_get
  let mtime = le_to_integer mtime
  xfl <- p_get

  let compression_factor =
      case xfl of
        2 => Just Strongest
        4 => Just Poorest
        _ => Nothing

  os <- from_id <$> p_get
  
  fextra <- ifA (testBit flag 2) $ do
    xlen <- p_nat 2
    toList <$> count xlen p_get

  fname <- ifA (testBit flag 3) nul_term_string

  fcomment <- ifA (testBit flag 4) nul_term_string

  fhcrc <- ifA (testBit flag 1) (count 2 p_get)

  pure (Just $ MkGZipHeader (cast mtime) compression_factor os fextra fname fcomment fhcrc)

export
parse_gzip_footer : (Cons (Posed Bits8) i, Monoid i) => Parser i (SimpleError String) GZipFooter
parse_gzip_footer = do
  crc32 <- cast <$> p_nat 4
  isize <- cast <$> p_nat 4
  pure (MkGZipFooter crc32 isize)
