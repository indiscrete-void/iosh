module Polysemy.Process
  ( Process (..),
    ProcessParams (..),
    exec,
    wait,
    read,
    readErr,
    write,
    reader,
    errReader,
    scopedProcToIO,
  )
where

import Control.Monad
import Data.ByteString (ByteString, hGetSome, hPut)
import Data.Kind
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

errReader :: (Member Process r) => Producer ByteString (Sem r) ()
errReader = P.repeatM readErr >-> justYielder

pipedProc :: FilePath -> Args -> CreateProcess
pipedProc path args =
  (proc path args)
    { std_in = CreatePipe,
      std_out = CreatePipe,
      std_err = CreatePipe
    }

scopedProcToIO :: (Member (Embed IO) r) => InterpreterFor (Scoped ProcessParams Process) r
scopedProcToIO = interpretScoped (\params f -> open params >>= \resource -> f resource <* close resource) procToIO
  where
    open (ProcessParams path args) = embed $ do
      (Just i, Just o, Just e, ph) <- createProcess (pipedProc path args)
      mapM_ (`hSetBuffering` NoBuffering) [i, o, e]
      pure (i, o, e, ph)
    procToIO :: (Member (Embed IO) r) => (Handle, Handle, Handle, ProcessHandle) -> Process m x -> Sem r x
    procToIO (_, _, _, ph) Wait = embed $ waitForProcess ph
    procToIO (_, o, _, _) Read = embed $ eofToNothing <$> hGetSome o 8192
    procToIO (_, _, e, _) ReadErr = embed $ eofToNothing <$> hGetSome e 8192
    procToIO (i, _, _, _) (Write str) = embed $ hPut i str
    close (i, o, e, ph) = embed $ cleanupProcess (Just i, Just o, Just e, ph)
