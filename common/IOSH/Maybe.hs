module IOSH.Maybe (eofToNothing, justYielder) where

import Data.ByteString
import Pipes
import Polysemy

eofToNothing :: ByteString -> Maybe ByteString
eofToNothing str =
  if str == empty
    then Nothing
    else Just str

justYielder :: Pipe (Maybe a) a (Sem r) ()
justYielder = await >>= maybe (pure ()) go
  where
    go a = yield a >> justYielder
