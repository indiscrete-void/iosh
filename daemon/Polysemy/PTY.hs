module Polysemy.PTY
  ( PTY (..),
    PTYParams (..),
    exec,
    wait,
    resize,
    read,
    write,
    reader,
    writer,
    scopedPTYToIO,
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
import Polysemy.Scoped
import Polysemy.Transport
import System.Exit
import System.Posix.Pty
import System.Process
import Prelude hiding (read)

type PTYParams :: Type
data PTYParams = PTYParams FilePath Args Size

type PTY :: Effect
data PTY m a where
  Wait :: PTY m ExitCode
  Resize :: Size -> PTY m ()
  Read :: PTY m (Maybe ByteString)
  Write :: ByteString -> PTY m ()

exec :: (Member (Scoped PTYParams PTY) r) => PTYParams -> InterpreterFor PTY r
exec = scoped

makeSem ''PTY

reader :: (Member PTY r) => Producer ByteString (Sem r) ()
reader = P.repeatM read >-> justYielder

writer :: (Member PTY r) => Consumer ByteString (Sem r) ()
writer = P.mapM_ write

ps2s :: Size -> (Int, Int)
ps2s = join bimap fromIntegral

scopedPTYToIO :: (Member (Embed IO) r) => InterpreterFor (Scoped PTYParams PTY) r
scopedPTYToIO = interpretScoped (\params f -> open params >>= \resource -> f resource <* close resource) ptyToIO
  where
    open (PTYParams path args size) = embed $ spawnWithPty Nothing True path args (ps2s size)
    ptyToIO :: (Member (Embed IO) r) => (Pty, ProcessHandle) -> PTY m x -> Sem r x
    ptyToIO (_, h) Wait = embed $ waitForProcess h
    ptyToIO (pty, _) (Resize size) = embed $ resizePty pty (ps2s size)
    ptyToIO (pty, _) Read = embed $ threadWaitReadPty pty >> ioErrorToNothing (readPty pty)
    ptyToIO (pty, _) (Write str) = embed $ threadWaitWritePty pty >> writePty pty str
    close (pty, _) = embed $ closePty pty
