module Polysemy.Transport
  ( ByteInput,
    ByteOutput,
    inputter,
    outputter,
    ioErrorToNothing,
    inputToIO,
    outputToIO,
    xInputter,
    xOutputter,
    inputX,
    outputX,
  )
where

import Control.Exception
import Data.ByteString
import Data.Kind
import Data.Serialize
import IOSH.Maybe
import Pipes hiding (embed)
import Pipes.Prelude qualified as P
import Polysemy
import Polysemy.Fail
import Polysemy.Input
import Polysemy.Output
import Polysemy.Serialize
import System.IO

type ByteInput :: (Type -> Type) -> Type -> Type
type ByteInput = Input (Maybe ByteString)

type ByteOutput :: (Type -> Type) -> Type -> Type
type ByteOutput = Output ByteString

inputter :: (Member (Input (Maybe a)) r) => Producer a (Sem r) ()
inputter = P.repeatM input >-> justYielder

outputter :: (Member (Output a) r) => Consumer a (Sem r) ()
outputter = P.mapM_ output

xInputter :: (Member ByteInput r, Member Decoder r, Serialize a, Member Fail r) => Producer a (Sem r) ()
xInputter = inputter >-> decoder

xOutputter :: (Member ByteOutput r, Serialize a) => Consumer a (Sem r) ()
xOutputter = P.map encode >-> outputter

inputX :: (Member ByteInput r, Member Decoder r, Member Fail r, Serialize a) => Sem r a
inputX = P.head xInputter >>= maybe (fail "end of pipe reached") pure

outputX :: (Member ByteOutput r, Serialize a) => a -> Sem r ()
outputX a = runEffect $ yield a >-> xOutputter

ioErrorToNothing :: IO a -> IO (Maybe a)
ioErrorToNothing m = either (const Nothing) Just <$> try @IOError m

inputToIO :: (Member (Embed IO) r) => Handle -> InterpreterFor ByteInput r
inputToIO h = interpret $ \case
  Input -> embed $ eofToNothing <$> hGetSome h 8192

outputToIO :: (Member (Embed IO) r) => Handle -> InterpreterFor ByteOutput r
outputToIO h = interpret $ \case
  (Output str) -> embed $ hPut h str
