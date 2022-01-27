module Test.Test

import Data.Compress.GZip
import Data.Compress.CRC
import System.File
import System.File.Mode
import Control.Monad.Error.Either
import Control.Monad.Trans
import Data.Buffer
import Data.List
import Data.Compress.Utils.Parser
import Data.Compress.Utils.Bytes
import Data.Vect
import Data.SnocList

%default partial

maybe_to_either : Lazy b -> Maybe a -> Either b a
maybe_to_either b Nothing = Left $ Force b
maybe_to_either _ (Just a) = Right a

readFile : File -> EitherT String IO (List Bits8)
readFile file = (MkEitherT $ map (maybe_to_either "canont create buffer") (newBuffer 1024)) >>= loop Lin where
  loop : SnocList Bits8 -> Buffer -> EitherT String IO (List Bits8)
  loop acc buffer = do
    False <- lift $ fEOF file
    | True => pure (toList acc)
    buffer_size <- lift (rawSize buffer)
    len <- bimapEitherT show id $ MkEitherT $ readBufferData file buffer 0 buffer_size
    data' <- traverse (getBits8 buffer) [0..(len-1)]
    loop (acc <>< data') buffer

test_gzip_file : String -> IO (Either String ())
test_gzip_file path = withFile path Read (pure . show) $ \file => runEitherT $ do
  content <- readFile file
  case feed content parse_gzip_header of
    Pure deflate_data _ => do
      let Right uncompressed = deflate_decompress deflate_data
      | Left err => printLn err
      putStrLn $ ascii_to_string uncompressed
      putStrLn ""
      -- putStrLn "crc32 match: \{show (footer.crc32 == crc32 uncompressed)}"
      -- putStrLn "isize match: \{show (footer.isize == (cast $ length uncompressed))}"
      -- putStrLn "\{show $ length uncompressed} read"
    Fail err =>
      printLn err
    _ => printLn "underfed"

test_deflate_uncompressed : IO (Either String ())
test_deflate_uncompressed = test_gzip_file "test/random.bin.gz"

test_deflate_fixed : IO (Either String ())
test_deflate_fixed = test_gzip_file "test/hello.gz"

test_deflate_text : IO (Either String ())
test_deflate_text = test_gzip_file "test/jabberwock.txt.gz"

test_deflate_jpg : IO (Either String ())
test_deflate_jpg = test_gzip_file "test/jabberwocky.jpg.gz"

export
test : IO ()
test = do
  Right () <- test_deflate_text
  | Left err => printLn err

  putStrLn "ok"
