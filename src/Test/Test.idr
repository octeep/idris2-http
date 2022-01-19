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
import Utils.Bytes
import Data.Vect

%default partial

maybe_to_either : Lazy b -> Maybe a -> Either b a
maybe_to_either b Nothing = Left $ Force b
maybe_to_either _ (Just a) = Right a

test_gzip_file : String -> IO (Either String ())
test_gzip_file path = withFile path Read (pure . show) $ \file => runEitherT $ do
  buffer <- MkEitherT $ map (maybe_to_either "canont create buffer") (newBuffer 204811)
  bimapEitherT show id $ MkEitherT $ readBufferData file buffer 0 204811
  file_len <- bimapEitherT show id $ MkEitherT $ fileSize file
  content <- lift $ traverse (getBits8 buffer) [0..(file_len - 1)]

  let result = feed (posify content) $ do
    _ <- parse_gzip_header
    body <- parse_deflate
    footer <- parse_gzip_footer
    pure (body, footer)

  case result of
    Pure leftover (uncompressed, footer) => do
      putStrLn $ ascii_to_string uncompressed
      printLn footer.isize
      putStrLn "exepected: \{show footer.crc32}"
      putStrLn "computed: \{show $ crc32 uncompressed}"
      putStrLn "\{show $ length uncompressed} read" 
    Fail err =>
      printLn err
    _ => printLn "underfed"

test_deflate_uncompressed : IO (Either String ())
test_deflate_uncompressed = test_gzip_file "test/random.bin.gz"

test_deflate_text : IO (Either String ())
test_deflate_text = test_gzip_file "test/config.gz"

test_deflate_jpg : IO (Either String ())
test_deflate_jpg = test_gzip_file "test/jabberwocky.jpg.gz"

export
test : IO ()
test = do
  {-
  Right () <- test_deflate_uncompressed
  | Left err => printLn err
  -}

  Right () <- test_deflate_text
  | Left err => printLn err

  putStrLn "ok"
