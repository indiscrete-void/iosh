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
    readX,
    xReader,
  )
where

import Data.ByteString (ByteString, hGetSome, hPut)
import Data.Kind
import Data.Maybe
import Data.Serialize
import IOSH.IO
import IOSH.Maybe
import IOSH.Protocol
import Pipes hiding (Effect)
import Pipes.Prelude qualified as P
import Polysemy
import Polysemy.Fail
import Polysemy.Resource
import Polysemy.Scoped
import Polysemy.Serialize
import System.Exit
import System.IO
import System.Process
import Prelude hiding (read)

type ProcessParams :: Type
data ProcessParams = InternalProcess (Maybe Environment) FilePath [String] | TunnelProcess String

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

xReader :: (Member Process r, Member Decoder r, Serialize a, Member Fail r) => Producer a (Sem r) ()
xReader = reader >-> decoder

xWriter :: (Member Process r, Serialize a) => Consumer a (Sem r) ()
xWriter = P.map encode >-> writer

readX :: (Member Process r, Member Decoder r, Member Fail r, Serialize a) => Sem r a
readX = P.head xReader >>= maybe (fail "end of pipe reached") pure

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
    openProc params = embedFinal $ createProcess (toCreateProcess params)
    disableProcBuffering (i, o, e, _) = mapM_ disableBuffering (catMaybes [i, o, e])
    procToIO :: (Member (Final IO) r) => (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> Process m x -> Sem r x
    procToIO (i, o, e, ph) = \case
      Wait -> embedFinal $ waitForProcess ph
      Read -> embedFinal $ eofToNothing <$> (maybeFail "failed to get output stream" o >>= flip hGetSome 8192)
      ReadErr -> embedFinal $ eofToNothing <$> (maybeFail "failed to get error stream" e >>= flip hGetSome 8192)
      (Write str) -> embedFinal (maybeFail "failed to get input stream" i >>= flip hPut str)
    closeProc hs = embedFinal $ cleanupProcess hs
    toCreateProcess (InternalProcess maybeEnv path args) = (proc path args) {env = maybeEnv, std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
    toCreateProcess (TunnelProcess cmd) = (shell cmd) {std_in = CreatePipe, std_out = CreatePipe}
