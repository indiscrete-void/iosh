module Polysemy.Transport
  ( ByteInput,
    ByteOutput,
    justYielder,
    inputter,
    outputter,
  )
where

import Data.ByteString
import Data.Kind
import Pipes
import Pipes.Prelude qualified as P
import Polysemy
import Polysemy.Input
import Polysemy.Output

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
