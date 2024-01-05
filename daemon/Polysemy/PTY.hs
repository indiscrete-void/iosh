module Polysemy.PTY
  ( PTY (..),
    exec,
    wait,
    resize,
    read,
    write,
    reader,
    writer,
  )
where

import Data.ByteString (ByteString)
import Data.Kind
import IOSH.Protocol
import Pipes
import Pipes.Prelude qualified as P
import Polysemy
import Polysemy.Transport
import System.Exit
import Prelude hiding (read)

type PTY :: Type -> k -> Type -> Type
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
