module Polysemy.User
  ( User (..),
    read,
    write,
    exit,
    reader,
    writer,
  )
where

import Data.ByteString
import Data.Kind
import Pipes
import Pipes.Prelude qualified as P
import Polysemy
import Polysemy.Transport
import System.Exit
import Prelude hiding (read)

type User :: k -> Type -> Type
data User m a where
  Read :: User m (Maybe ByteString)
  Write :: ByteString -> User m ()
  Exit :: ExitCode -> User m () -- exit >> m = exit

makeSem ''User

reader :: (Member User r) => Producer ByteString (Sem r) ()
reader = P.repeatM read >-> justYielder

writer :: (Member User r) => Consumer ByteString (Sem r) ()
writer = P.mapM_ write
