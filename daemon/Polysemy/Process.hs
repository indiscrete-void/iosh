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
    scopedProcToIOFinal,
  )
where

import Data.ByteString (ByteString, hGetSome, hPut)
import Data.Kind
import IOSH.Process
import IOSH.Protocol hiding (Resize)
import Pipes hiding (Effect)
import Pipes.Prelude qualified as P
import Polysemy
import Polysemy.Resource
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

scopedProcToIOFinal :: (Member (Final IO) r) => InterpreterFor (Scoped ProcessParams Process) r
scopedProcToIOFinal =
  interpretScoped
    ( \params f -> resourceToIOFinal . bracket (open params) close $
        \hs -> embedFinal (disableProcessBuffering hs) >> raise (f hs)
    )
    procToIO
  where
    open (ProcessParams path args) = embedFinal $ openProcess (proc path args)
    procToIO :: (Member (Final IO) r) => ProcessHandles -> Process m x -> Sem r x
    procToIO (i, o, e, ph) = \case
      Wait -> embedFinal $ waitForProcess ph
      Read -> embedFinal $ eofToNothing <$> hGetSome o 8192
      ReadErr -> embedFinal $ eofToNothing <$> hGetSome e 8192
      (Write str) -> embedFinal $ hPut i str
    close hs = embedFinal $ closeProcess hs
