module Polysemy.Serialize
  ( Decoder,
    runDecoder,
    serializeOutput,
    deserializeInput,
  )
where

import Data.ByteString (ByteString)
import Data.Serialize
import Data.Serialize qualified as Serial
import Polysemy hiding (send)
import Polysemy.Extra
import Polysemy.Fail
import Polysemy.Input
import Polysemy.Output
import Polysemy.State as State
import Polysemy.Transport

type Decoder :: Polysemy.Effect
type Decoder = State (Maybe ByteString)

runDecoder :: Sem (Decoder : r) a -> Sem r a
runDecoder = evalState Nothing

deserialize :: forall a r. (Member Decoder r, Serialize a, Member Fail r, Member ByteInputWithEOF r) => Sem r a
deserialize = takeState >>= maybe inputOrFail pure >>= go . runGetPartial Serial.get
  where
    takeState = State.get <* State.put @(Maybe ByteString) Nothing
    putJust = State.put . Just
    go (Serial.Fail msg left) = putJust left >> fail msg
    go (Done a left) = putJust left >> pure a
    go (Partial f) = inputOrFail >>= go . f

deserializeInput :: forall a r. (Serialize a, Member Decoder r, Member Fail r, Member (InputWithEOF ByteString) r) => InterpreterFor (InputWithEOF a) r
deserializeInput = interpret \case
  Input -> Just <$> (deserialize @a)

serializeOutput :: forall a r. (Serialize a, Member (Output ByteString) r) => InterpreterFor (Output a) r
serializeOutput = mapOutput encode
