module Polysemy.Process
  ( Process (..),
    ProcessParams (..),
    exec,
    wait,
    read,
    readErr,
    write,
    reader,
    writer,
    errReader,
    scopedProcToIO,
  )
where

import Control.Monad
import Data.ByteString (ByteString, hGetSome, hPut)
import Data.Kind
import IOSH.Maybe
import IOSH.Protocol hiding (Resize)
import Pipes hiding (Effect, embed)
import Pipes.Prelude qualified as P
import Polysemy
import Polysemy.Scoped
import Polysemy.Transport
import System.Exit
import System.IO
import System.Process
import Prelude hiding (read)

type ProcessParams :: Type
data ProcessParams = ProcessParams FilePath Args

type Process :: Effect
data Process m a where
  Wait :: Process m ExitCode
  Read :: Process m (Maybe ByteString)
  ReadErr :: Process m (Maybe ByteString)
  Write :: ByteString -> Process m ()

exec :: (Member (Scoped ProcessParams Process) r) => ProcessParams -> InterpreterFor Process r
exec = scoped

makeSem ''Process

reader :: (Member Process r) => Producer ByteString (Sem r) ()
reader = P.repeatM read >-> justYielder

writer :: (Member Process r) => Consumer ByteString (Sem r) ()
writer = P.mapM_ write

errReader :: (Member Process r) => Producer ByteString (Sem r) ()
errReader = P.repeatM readErr >-> justYielder

pipedProc :: FilePath -> Args -> CreateProcess
pipedProc path args =
  (proc path args)
    { std_in = CreatePipe,
      std_out = CreatePipe,
      std_err = CreatePipe
    }

unMaybeStreams :: (MonadFail m) => (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> m (Handle, Handle, Handle, ProcessHandle)
unMaybeStreams (i, o, e, h) = do
  i' <- maybeFail "unable to get stdin" i
  o' <- maybeFail "unable to get stderr" o
  e' <- maybeFail "unable to get stdout" e
  pure (i', o', e', h)

scopedProcToIO :: (Member (Embed IO) r) => InterpreterFor (Scoped ProcessParams Process) r
scopedProcToIO = interpretScoped (\params f -> open params >>= \resource -> f resource <* close resource) procToIO
  where
    open (ProcessParams path args) = embed $ createProcess (pipedProc path args) >>= unMaybeStreams
    procToIO :: (Member (Embed IO) r) => (Handle, Handle, Handle, ProcessHandle) -> Process m x -> Sem r x
    procToIO (_, _, _, h) Wait = embed $ waitForProcess h
    procToIO (_, o, _, _) Read = embed $ eofToNothing <$> hGetSome o 8192
    procToIO (_, _, e, _) ReadErr = embed $ eofToNothing <$> hGetSome e 8192
    procToIO (i, _, _, _) (Write str) = embed $ hPut i str
    close (i, o, e, h) = embed $ hClose i >> hClose o >> hClose e >> terminateProcess h
