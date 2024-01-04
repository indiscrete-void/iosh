module Polysemy.Transport.IO
  ( eofToNothing,
    ioErrorToNothing,
    inputToIO,
    outputToIO,
  )
where

import Control.Exception
import Data.ByteString
import Polysemy
import Polysemy.Input
import Polysemy.Output
import Polysemy.Transport
import System.IO

eofToNothing :: ByteString -> Maybe ByteString
eofToNothing str =
  if str == empty
    then Nothing
    else Just str

ioErrorToNothing :: IO a -> IO (Maybe a)
ioErrorToNothing m = either (const Nothing) Just <$> try @IOError m

inputToIO :: (Member (Embed IO) r) => Handle -> InterpreterFor ByteInput r
inputToIO h = interpret $ \case
  Input -> embed $ eofToNothing <$> hGetSome h 8192

outputToIO :: (Member (Embed IO) r) => Handle -> InterpreterFor ByteOutput r
outputToIO h = interpret $ \case
  (Output str) -> embed $ hPut h str
