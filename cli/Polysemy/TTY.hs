module Polysemy.TTY
  ( TTY (..),
    getSize,
    setResizeHandler,
    attributeBracket,
    setRawAttributes,
    ttyToIOFinal,
  )
where

import Control.Exception
import Control.Monad
import Data.Int
import IOSH.Maybe
import IOSH.Protocol
import Polysemy
import Polysemy.Final
import System.Console.Terminal.Size
import System.Posix.ByteString
import System.Posix.Signals.Exts
import Prelude hiding (read)

type TTY :: Effect
data TTY m a where
  GetSize :: TTY m (Maybe Size)
  SetResizeHandler :: (Size -> m ()) -> TTY m ()
  AttributeBracket :: m a -> TTY m a
  SetRawAttributes :: TTY m ()

makeSem ''TTY

ttyToIOFinal :: (Member (Final IO) r) => Fd -> InterpreterFor TTY r
ttyToIOFinal term = interpretFinal @IO $ \case
  (SetResizeHandler f) -> wrapHandlerS >>= liftS . go
    where
      go f' = void $ installHandler sigWINCH (Catch f') Nothing
      wrapHandlerS = liftM3 wrapper (bindS f) getInspectorS getInitialStateS
        where
          wrapper f' ins s = (protoSize >>= maybe (pure . void $ s) (f' . (<$ s))) >>= maybeFail "unable to inspect SIGWINCH handler result" . inspect ins
  GetSize -> liftS protoSize
  (AttributeBracket m) -> wrapBracketActionS >>= liftS . go
    where
      go m' = bracket (getTerminalAttributes term) (setTerminalAttributesImmediately term) (const m')
      wrapBracketActionS = liftM2 wrapper (runS m) getInspectorS
        where
          wrapper m' ins = m' >>= maybeFail "unable to inspect bracket action result" . inspect ins
  SetRawAttributes -> liftS $ getTerminalAttributes term >>= setTerminalAttributesImmediately term . withRaw
    where
      withRaw attrs = foldr (flip withoutMode) attrs [EnableEcho, IgnoreBreak, InterruptOnBreak, IgnoreParityErrors, MarkParityErrors, CheckParity, StripHighBit, MapLFtoCR, IgnoreCR, MapCRtoLF, StartStopOutput, StartStopInput, ProcessInput, ProcessOutput, KeyboardInterrupts]
  where
    setTerminalAttributesImmediately fd attrs = setTerminalAttributes fd attrs Immediately
    protoSize = fmap w2s <$> size @Int16
      where
        w2s (Window h w) = (w, h)
