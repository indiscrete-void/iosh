module Polysemy.TTY.IO (ttyToIOFinal) where

import Control.Monad
import Data.ByteString
import IOSH.Protocol
import Polysemy
import Polysemy.Final
import Polysemy.TTY
import Polysemy.Transport.IO
import System.Console.Terminal.Size
import System.Exit
import System.IO
import System.Posix.Signals.Exts

maybeFail :: (MonadFail m) => String -> Maybe a -> m a
maybeFail str = maybe (fail str) pure

protoSize :: IO Size
protoSize = maybeProtoSize >>= maybeFail "unable to get terminal size"
  where
    maybeProtoSize = fmap w2s <$> size
      where
        w2s (Window h w) = (w, h)

wrapHandlerS :: (Functor f) => (Size -> n b) -> Sem (WithStrategy IO f n) (IO b)
wrapHandlerS f = liftM3 wrapper (bindS f) getInspectorS getInitialStateS
  where
    wrapper h ins s = protoSizeS >>= maybeFail "unable to inspect SIGWINCH handler result"
      where
        protoSizeS = inspect ins <$> (protoSize >>= h . (<$ s))

ttyToIOFinal :: (Member (Final IO) r) => Handle -> Handle -> InterpreterFor TTY r
ttyToIOFinal i o = interpretFinal @IO $ \case
  (SetSizeChH f) -> wrapHandlerS f >>= liftS . go
    where
      go g = void $ installHandler sigWINCH (Catch g) Nothing
  (SetEcho b) -> liftS $ hSetEcho i b
  GetSize -> liftS protoSize
  Read -> liftS $ eofToNothing <$> hGetSome i 8192
  (Write str) -> liftS $ hPut o str
  (Exit code) -> liftS $ exitWith code
