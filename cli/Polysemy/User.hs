module Polysemy.User
  ( User (..),
    read,
    write,
    writeErr,
    exit,
    reader,
    userToIO,
  )
where

import Data.ByteString
import Pipes hiding (Effect, embed)
import Pipes.Prelude qualified as P
import Polysemy
import IOSH.Maybe
import System.Exit
import System.Posix.ByteString
import Prelude hiding (read)

type User :: Effect
data User m a where
  Read :: User m (Maybe ByteString)
  Write :: ByteString -> User m ()
  WriteErr :: ByteString -> User m ()
  Exit :: ExitCode -> User m () -- exit >> m = exit

makeSem ''User

reader :: (Member User r) => Producer ByteString (Sem r) ()
reader = P.repeatM read >-> justYielder

userToIO :: (Member (Embed IO) r) => Fd -> Fd -> Fd -> InterpreterFor User r
userToIO i o e = interpret $ \case
  Read -> embed $ eofToNothing <$> fdRead i 8192
  (Write str) -> embed . void $ fdWrite o str
  (WriteErr str) -> embed . void $ fdWrite e str
  (Exit code) -> embed $ exitWith code
