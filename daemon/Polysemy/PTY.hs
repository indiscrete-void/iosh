module Polysemy.PTY
  ( PTY (..),
    PTYHandle,
    exec,
    wait,
    resize,
    read,
    write,
    reader,
    writer,
    ptyToIO,
  )
where

import Control.Monad
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.Kind
import IOSH.Protocol hiding (Resize)
import Pipes hiding (Effect, embed)
import Pipes.Prelude qualified as P
import Polysemy
import Polysemy.Transport
import System.Exit
import System.Posix.Pty
import System.Process
import Prelude hiding (read)

type PTY :: Type -> Effect
data PTY h m a where
  Exec :: FilePath -> Args -> Size -> PTY h m h
  Wait :: h -> PTY h m ExitCode
  Resize :: h -> Size -> PTY h m ()
  Read :: h -> PTY h m (Maybe ByteString)
  Write :: h -> ByteString -> PTY h m ()

makeSem ''PTY

reader :: (Member (PTY h) r) => h -> Producer ByteString (Sem r) ()
reader h = P.repeatM (read h) >-> justYielder

writer :: (Member (PTY h) r) => h -> Consumer ByteString (Sem r) ()
writer h = P.mapM_ $ write h

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
