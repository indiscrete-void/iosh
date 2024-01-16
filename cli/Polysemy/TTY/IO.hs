module Polysemy.TTY.IO (ttyToIOFinal) where

import Control.Exception
import Control.Monad
import IOSH.Protocol
import Polysemy
import Polysemy.Final
import Polysemy.TTY
import System.Console.Terminal.Size
import System.Posix hiding (fdRead, fdWrite)
import System.Posix.Signals.Exts

maybeFail :: (MonadFail m) => String -> Maybe a -> m a
maybeFail str = maybe (fail str) pure

protoSize :: IO Size
protoSize = maybeProtoSize >>= maybeFail "unable to get terminal size"
  where
    maybeProtoSize = fmap w2s <$> size
      where
        w2s (Window h w) = (w, h)

wrapHandlerS :: (Functor f) => (Size -> n a) -> Sem (WithStrategy IO f n) (IO a)
wrapHandlerS f = liftM3 wrapper (bindS f) getInspectorS getInitialStateS
  where
    wrapper f' ins s = (protoSize >>= f' . (<$ s)) >>= maybeFail "unable to inspect SIGWINCH handler result" . inspect ins

wrapBracketActionS :: n a -> Sem (WithStrategy IO f n) (IO a)
wrapBracketActionS m = liftM2 wrapper (runS m) getInspectorS
  where
    wrapper m' ins = m' >>= maybeFail "unable to inspect bracket action result" . inspect ins

withRaw :: TerminalAttributes -> TerminalAttributes
withRaw attrs = foldr (flip withoutMode) attrs [EnableEcho, IgnoreBreak, InterruptOnBreak, IgnoreParityErrors, MarkParityErrors, CheckParity, StripHighBit, MapLFtoCR, IgnoreCR, MapCRtoLF, StartStopOutput, StartStopInput, ProcessInput, ProcessOutput, KeyboardInterrupts]

setTerminalAttributesImmediately :: Fd -> TerminalAttributes -> IO ()
setTerminalAttributesImmediately fd attrs = setTerminalAttributes fd attrs Immediately

ttyToIOFinal :: (Member (Final IO) r) => Fd -> InterpreterFor TTY r
ttyToIOFinal term = interpretFinal @IO $ \case
  (SetResizeHandler f) -> wrapHandlerS f >>= liftS . go
    where
      go f' = void $ installHandler sigWINCH (Catch f') Nothing
  GetSize -> liftS protoSize
  (AttributeBracket m) -> wrapBracketActionS m >>= liftS . go
    where
      go m' = bracket (getTerminalAttributes term) (setTerminalAttributesImmediately term) (const m')
  SetRaw -> liftS $ getTerminalAttributes term >>= setTerminalAttributesImmediately term . withRaw
