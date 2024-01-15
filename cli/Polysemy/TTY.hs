module Polysemy.TTY
  ( TTY (..),
    getSize,
    setResizeHandler,
    read,
    write,
    exit,
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

type TTY :: (Type -> Type) -> Type -> Type
data TTY m a where
  GetSize :: TTY m Size
  SetResizeHandler :: (Size -> m ()) -> TTY m ()
  Read :: TTY m (Maybe ByteString)
  Write :: ByteString -> TTY m ()
  Exit :: ExitCode -> TTY m () -- exit >> m = exit

makeSem ''TTY

reader :: (Member TTY r) => Producer ByteString (Sem r) ()
reader = P.repeatM read >-> justYielder

writer :: (Member TTY r) => Consumer ByteString (Sem r) ()
writer = P.mapM_ write
