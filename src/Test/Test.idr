module Test.Test

import Data.Compress.GZip
import Data.Compress.CRC
import System.File
import System.File.Mode
import Control.Monad.Error.Either
import Control.Monad.Trans
import Data.Buffer
import Data.List
import Data.Compress.Utils.Bitstream
import Utils.Bytes
import Data.Vect
import Utils.Streaming
import System.File.Support

%default partial

%foreign support "idris2_readBufferData"
         "node:lambda:(f,b,l,m) => require('fs').readSync(f.fd,b,l,m)"
prim__readBufferData : FilePtr -> Buffer -> (offset : Int) -> (maxbytes : Int) -> PrimIO Int

readBufferData : HasIO io => (fh : File) -> (buf : Buffer) ->
                 (offset : Int) ->
                 (maxbytes : Int) ->
                 io (Either FileError Int)
readBufferData (FHandle h) buf offset max
    = do read <- primIO (prim__readBufferData h buf offset max)
         if read >= 0
            then pure (Right read)
            else pure (Left FileReadError)

||| Construct a `Stream` reading from a File
export
fromFile : HasIO m => File -> Stream (Of Bits8) m (Either FileError ())
fromFile file = (newBuffer 8192) >>= loop where
  loop : Maybe Buffer -> Stream (Of Bits8) m (Either FileError ())
  loop (Just buffer) = do
    False <- liftIO $ fEOF file
    | True => pure (Right ())
    Right cap <- liftIO (rawSize buffer >>= Test.readBufferData file buffer 0)
    | Left err => pure (Left err)
    data' <- traverse (getBits8 buffer) [0..(cap-1)]
    fromList_ data' *> loop (Just buffer)

maybe_to_either : Lazy b -> Maybe a -> Either b a
maybe_to_either b Nothing = Left $ Force b
maybe_to_either _ (Just a) = Right a

test_gzip_file : String -> IO (Either String ())
test_gzip_file path = withFile path Read (pure . show) $ \file => runEitherT $ do
  (uncompressed, thing) <- lift $ toList $ decompress_gzip (fromFile file)
  printLn thing
  putStrLn $ ascii_to_string uncompressed
  -- printLn footer.isize
  -- putStrLn "exepected: \{show footer.crc32}"
  -- putStrLn "computed: \{show $ crc32 uncompressed}"
  putStrLn "\{show $ length uncompressed} read" 

test_deflate_uncompressed : IO (Either String ())
test_deflate_uncompressed = test_gzip_file "test/random.bin.gz"

test_deflate_text : IO (Either String ())
test_deflate_text = test_gzip_file "test/jabberwock.txt.gz"

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
