module Polysemy.PTY.IO (PTYHandle, ptyToIO) where

import Control.Monad
import Data.Bifunctor
import Data.Kind
import IOSH.Protocol hiding (Resize)
import Polysemy
import Polysemy.PTY
import Polysemy.Transport.IO
import System.Posix.Pty
import System.Process

type PTYHandle :: Type
type PTYHandle = (Pty, ProcessHandle)

ps2s :: Size -> (Int, Int)
ps2s = join bimap fromIntegral

ptyToIO :: (Member (Embed IO) r) => InterpreterFor (PTY PTYHandle) r
ptyToIO = interpret $ \case
  (Exec path args size) -> embed $ spawnWithPty Nothing True path args (ps2s size)
  (Wait (_, h)) -> embed $ waitForProcess h
  (Resize (pty, _) size) -> embed $ resizePty pty (ps2s size)
  (Read (pty, _)) -> embed $ threadWaitReadPty pty >> ioErrorToNothing (readPty pty)
  (Write (pty, _) str) -> embed $ threadWaitWritePty pty >> writePty pty str
  (Close (pty, _)) -> embed $ closePty pty
