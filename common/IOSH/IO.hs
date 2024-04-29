module IOSH.IO (disableBuffering, bufferSize) where

import System.IO

disableBuffering :: Handle -> IO ()
disableBuffering = (`hSetBuffering` NoBuffering)

bufferSize :: Int
bufferSize = 8192
