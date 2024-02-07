module Polysemy.PTY
  ( PTY (..),
    PTYParams (..),
    exec,
    wait,
    resize,
    read,
    write,
    reader,
    scopedPTYToIOFinal,
  )
where

import Control.Monad
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.Kind
import IOSH.Protocol hiding (Resize)
import Pipes hiding (Effect)
import Pipes.Prelude qualified as P
import Polysemy
import Polysemy.Resource
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

ps2s :: Size -> (Int, Int)
ps2s = join bimap fromIntegral

scopedPTYToIOFinal :: (Member (Final IO) r) => InterpreterFor (Scoped PTYParams PTY) r
scopedPTYToIOFinal = interpretScoped (\params f -> resourceToIOFinal $ bracket (open params) close (raise . f)) ptyToIO
  where
    open (PTYParams path args size) = embedFinal $ spawnWithPty Nothing True path args (ps2s size)
    ptyToIO :: (Member (Final IO) r) => (Pty, ProcessHandle) -> PTY m x -> Sem r x
    ptyToIO (pty, ph) = \case
      Wait -> embedFinal $ waitForProcess ph
      (Resize size) -> embedFinal $ resizePty pty (ps2s size)
      Read -> embedFinal $ threadWaitReadPty pty >> ioErrorToNothing (readPty pty)
      (Write str) -> embedFinal $ threadWaitWritePty pty >> writePty pty str
    close (pty, _) = embedFinal $ closePty pty
