module Polysemy.Serialize
  ( Decoder,
    runDecoder,
    decoder,
  )
where

import Data.ByteString (ByteString)
import Data.Serialize
import Data.Serialize qualified as Serial
import Pipes
import Polysemy hiding (send)
import Polysemy.Fail
import Polysemy.State as State

type Decoder :: Polysemy.Effect
type Decoder = State (Maybe ByteString)

runDecoder :: Sem (Decoder : r) a -> Sem r a
runDecoder = evalState Nothing

decoder :: (Member Decoder r, Serialize a, Member Fail r) => Pipe ByteString a (Sem r) ()
decoder = takeState >>= maybe await pure >>= go . runGetPartial Serial.get
  where
    takeState = lift State.get <* lift (State.put @(Maybe ByteString) Nothing)
    putJust = lift . State.put . Just
    go (Serial.Fail msg left) = putJust left >> fail msg
    go (Done a left) = putJust left >> yield a >> decoder
    go (Partial f) = await >>= go . f
