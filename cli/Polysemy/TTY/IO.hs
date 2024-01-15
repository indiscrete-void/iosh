module Polysemy.TTY.IO (ttyToIOFinal) where

import Control.Exception
import Control.Monad
import IOSH.Protocol
import Polysemy
import Polysemy.Final
import Polysemy.TTY
import Polysemy.Transport.IO
import System.Console.Terminal.Size
import System.Exit
import System.Posix hiding (fdRead, fdWrite)
import System.Posix.IO.ByteString
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

wrapBracketActionS :: n a -> Sem (WithStrategy IO f n) (IO a)
wrapBracketActionS ma = liftM2 wrapper (runS ma) getInspectorS
  where
    wrapper mb ins = mb >>= maybeFail "unable to inspect bracket action result" . inspect ins

withRaw :: TerminalAttributes -> TerminalAttributes
withRaw attrs = foldr (flip withoutMode) attrs [EnableEcho, IgnoreBreak, InterruptOnBreak, IgnoreParityErrors, MarkParityErrors, CheckParity, StripHighBit, MapLFtoCR, IgnoreCR, MapCRtoLF, StartStopOutput, StartStopInput, ProcessInput, ProcessOutput, KeyboardInterrupts]

setTerminalAttributesImmediately :: Fd -> TerminalAttributes -> IO ()
setTerminalAttributesImmediately fd attrs = setTerminalAttributes fd attrs Immediately

ttyToIOFinal :: (Member (Final IO) r) => Fd -> Fd -> InterpreterFor TTY r
ttyToIOFinal i o = interpretFinal @IO $ \case
  (SetResizeHandler f) -> wrapHandlerS f >>= liftS . go
    where
      go g = void $ installHandler sigWINCH (Catch g) Nothing
  GetSize -> liftS protoSize
  (AttributeBracket ma) -> wrapBracketActionS ma >>= liftS . go
    where
      go mb = bracket (getTerminalAttributes i) (setTerminalAttributesImmediately i) (const mb)
  SetRaw -> liftS $ getTerminalAttributes i >>= setTerminalAttributesImmediately i . withRaw
  Read -> liftS $ eofToNothing <$> fdRead i 8192
  (Write str) -> liftS . void $ fdWrite o str
  (Exit code) -> liftS $ exitWith code
