module Polysemy.Serialize
  ( Decoder,
    runDecoder,
    deserialize,
    serialize,
    serializeOutput,
    deserializeInput,
  )
where

import Data.ByteString (ByteString)
import Data.Serialize
import Data.Serialize qualified as Serial
import Polysemy hiding (send)
import Polysemy.Fail
import Polysemy.Input
import Polysemy.Output
import Polysemy.State as State
import Transport.Polysemy

type Decoder :: Polysemy.Effect
type Decoder = State (Maybe ByteString)

runDecoder :: Sem (Decoder ': r) a -> Sem r a
runDecoder = evalState Nothing

deserialize :: forall a r. (Member Decoder r, Serialize a, Member Fail r, Member ByteInputWithEOF r) => Sem r a
deserialize = takeState >>= maybe inputOrFail pure >>= go . runGetPartial Serial.get
  where
    takeState = State.get <* State.put @(Maybe ByteString) Nothing
    putJust = State.put . Just
    go (Serial.Fail msg left) = putJust left >> fail msg
    go (Done a left) = putJust left >> pure a
    go (Partial f) = inputOrFail >>= go . f

serialize :: (Serialize a) => a -> ByteString
serialize = encode

deserializeInput :: forall a r. (Serialize a, Member Decoder r, Member Fail r, Member (InputWithEOF ByteString) r) => InterpreterFor (InputWithEOF a) r
deserializeInput = interpret \case
  Input -> Just <$> (deserialize @a)

serializeOutput :: forall a r. (Serialize a, Member (Output ByteString) r) => InterpreterFor (Output a) r
serializeOutput = mapOutput serialize

mapOutput :: (Member (Output o') r) => (o -> o') -> Sem (Output o ': r) a -> Sem r a
mapOutput f = interpret \case
  Output o -> output (f o)
