module Polysemy.Transport
  ( ByteInput,
    ByteOutput,
    justYielder,
    inputter,
    outputter,
    eofToNothing,
    ioErrorToNothing,
    inputToIO,
    outputToIO,
  )
where

import Control.Exception
import Data.ByteString
import Data.Kind
import Pipes hiding (embed)
import Pipes.Prelude qualified as P
import Polysemy
import Polysemy.Input
import Polysemy.Output
import System.IO

type ByteInput :: (Type -> Type) -> Type -> Type
type ByteInput = Input (Maybe ByteString)

type ByteOutput :: (Type -> Type) -> Type -> Type
type ByteOutput = Output ByteString

justYielder :: Pipe (Maybe a) a (Sem r) ()
justYielder = await >>= maybe (pure ()) go
  where
    go a = yield a >> justYielder

inputter :: (Member (Input (Maybe a)) r) => Producer a (Sem r) ()
inputter = P.repeatM input >-> justYielder

outputter :: (Member (Output a) r) => Consumer a (Sem r) ()
outputter = P.mapM_ output

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
