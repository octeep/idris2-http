module Test.WriteBuffer

import Data.Buffer
import System.File

%default partial

put : Show a => IO a -> IO ()
put = (>>= putStrLn . show)

read : IO ()
read = do
  Just writeBuf <- newBuffer 8
  | Nothing => pure ()
  put (rawSize writeBuf)
  -- setInt writeBuf 0 0x7766554433221100
  Right f <- openFile "/dev/zero" Read
  | Left err => put $ pure err
  Right ok <- readBufferData f writeBuf 0 8192
  | Left err => put $ pure err
  putStrLn "ok"

main : IO ()
main = do
  Just writeBuf <- newBuffer 256
  | Nothing => pure ()
  put (rawSize writeBuf)
  -- setInt writeBuf 0 0x7766554433221100
  Right f <- openFile "testWrite.buf" WriteTruncate
  | Left err => put $ pure err
  Right () <- writeBufferData f writeBuf 256 256
  | Left err => put $ pure err
  putStrLn "ok"
