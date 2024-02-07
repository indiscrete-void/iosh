module IOSH.IO (disableBuffering) where

import System.IO

disableBuffering :: Handle -> IO ()
disableBuffering = (`hSetBuffering` NoBuffering)
