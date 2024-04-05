module Polysemy.Transport
  ( InputWithEOF,
    ByteInput,
    ByteOutput,
    inputter,
    outputter,
    ioErrorToNothing,
    inputToIO,
    outputToIO,
    inputOrFail,
  )
where

import Control.Exception
import Data.ByteString
import Data.Kind
import IOSH.Maybe
import Pipes hiding (Effect, embed)
import Pipes.Prelude qualified as P
import Polysemy
import Polysemy.Fail
import Polysemy.Input
import Polysemy.Output
import System.IO

type InputWithEOF :: Type -> Effect
type InputWithEOF i = Input (Maybe i)

inputOrFail :: (Member (InputWithEOF a) r, Member Fail r) => Sem r a
inputOrFail = input >>= maybe (fail "eof reached") pure

type ByteInput :: Effect
type ByteInput = InputWithEOF ByteString

type ByteOutput :: Effect
type ByteOutput = Output ByteString

inputter :: (Member (InputWithEOF a) r) => Producer a (Sem r) ()
inputter = P.repeatM input >-> justYielder

outputter :: (Member (Output a) r) => Consumer a (Sem r) ()
outputter = P.mapM_ output

ioErrorToNothing :: IO a -> IO (Maybe a)
ioErrorToNothing m = either (const Nothing) Just <$> try @IOError m

inputToIO :: (Member (Embed IO) r) => Handle -> InterpreterFor ByteInput r
inputToIO h = interpret $ \case
  Input -> embed $ eofToNothing <$> hGetSome h 8192

outputToIO :: (Member (Embed IO) r) => Handle -> InterpreterFor ByteOutput r
outputToIO h = interpret $ \case
  (Output str) -> embed $ hPut h str
