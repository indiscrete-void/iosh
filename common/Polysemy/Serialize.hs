module Polysemy.Serialize
  ( CarriedOverByteString,
    runDecoder,
    inputX,
    outputX,
    xInputter,
    xOutputter,
  )
where

import Data.ByteString (ByteString)
import Data.Kind
import Data.Serialize
import Data.Serialize qualified as Serial
import Pipes
import Pipes.Prelude qualified as P
import Polysemy hiding (Effect, send)
import Polysemy.Fail
import Polysemy.Output
import Polysemy.State as State
import Polysemy.Transport

type CarriedOverByteString :: Type
type CarriedOverByteString = Maybe ByteString

runDecoder :: Sem (State CarriedOverByteString : r) a -> Sem r a
runDecoder = evalState Nothing

decoder :: (Member (State CarriedOverByteString) r, Serialize a) => Pipe ByteString a (Sem r) ()
decoder = takeState >>= maybe await pure >>= go . runGetPartial Serial.get
  where
    takeState = lift State.get <* lift (State.put @CarriedOverByteString Nothing)
    putJust = lift . State.put . Just
    --
    go (Serial.Fail _ left) = putJust left
    go (Done a left) = putJust left >> yield a >> decoder
    go (Partial f) = await >>= go . f

xInputter :: (Member ByteInput r, Member (State CarriedOverByteString) r, Serialize a) => Producer a (Sem r) ()
xInputter = inputter >-> decoder

xOutputter :: (Member ByteOutput r, Serialize a) => Consumer a (Sem r) ()
xOutputter = P.map encode >-> outputter

inputX :: (Member ByteInput r, Member (State CarriedOverByteString) r, Member Fail r, Serialize a) => Sem r a
inputX = P.head xInputter >>= maybe (fail "end of pipe reached") pure

outputX :: (Member ByteOutput r, Serialize a) => a -> Sem r ()
outputX a = output (encode a)
