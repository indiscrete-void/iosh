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
    writer,
    scopedProcToIOFinal,
    xWriter,
    writeX,
    xReader,
  )
where

import Data.ByteString (ByteString, hGetSome, hPut)
import Data.Kind
import Data.Serialize
import IOSH.IO
import IOSH.Maybe
import IOSH.Protocol
import Pipes hiding (Effect)
import Pipes.Prelude qualified as P
import Polysemy
import Polysemy.Resource
import Polysemy.Scoped
import Polysemy.Serialize
import System.Exit
import System.IO
import System.Process
import Prelude hiding (read)

type ProcessParams :: Type
data ProcessParams = EnvPathArgsProcess (Maybe Environment) FilePath [String] | ShellProcess String

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

writer :: (Member Process r) => Consumer ByteString (Sem r) ()
writer = P.mapM_ write

xReader :: (Member Process r, Member Decoder r, Serialize a) => Producer a (Sem r) ()
xReader = reader >-> decoder

xWriter :: (Member Process r, Serialize a) => Consumer a (Sem r) ()
xWriter = P.map encode >-> writer

writeX :: (Member Process r, Serialize a) => a -> Sem r ()
writeX a = runEffect $ yield a >-> xWriter

scopedProcToIOFinal :: (Member (Final IO) r) => InterpreterFor (Scoped ProcessParams Process) r
scopedProcToIOFinal =
  interpretScoped
    ( \params f -> resourceToIOFinal . bracket (openProc params) closeProc $
        \hs -> embedFinal (disableProcBuffering hs) >> raise (f hs)
    )
    procToIO
  where
    openProc params = embedFinal $ do
      (Just i, Just o, Just e, ph) <- createProcess ((toCreateProcess params) {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe})
      pure (i, o, e, ph)
    disableProcBuffering (i, o, e, _) = mapM_ disableBuffering [i, o, e]
    procToIO :: (Member (Final IO) r) => (Handle, Handle, Handle, ProcessHandle) -> Process m x -> Sem r x
    procToIO (i, o, e, ph) = \case
      Wait -> embedFinal $ waitForProcess ph
      Read -> embedFinal $ eofToNothing <$> hGetSome o 8192
      ReadErr -> embedFinal $ eofToNothing <$> hGetSome e 8192
      (Write str) -> embedFinal $ hPut i str
    closeProc (i, o, e, ph) = embedFinal $ cleanupProcess (Just i, Just o, Just e, ph)
    toCreateProcess (EnvPathArgsProcess maybeEnv path args) = (proc path args) {env = maybeEnv}
    toCreateProcess (ShellProcess cmd) = shell cmd
