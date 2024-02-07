module IOSH.Maybe (maybeFail, eofToNothing, justYielder) where

import Data.ByteString
import Pipes
import Polysemy

maybeFail :: (MonadFail m) => String -> Maybe a -> m a
maybeFail str = maybe (fail str) pure

eofToNothing :: ByteString -> Maybe ByteString
eofToNothing str =
  if str == empty
    then Nothing
    else Just str

justYielder :: Pipe (Maybe a) a (Sem r) ()
justYielder = await >>= maybe (pure ()) go
  where
    go a = yield a >> justYielder
