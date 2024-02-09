module Polysemy.User
  ( User (..),
    read,
    write,
    writeErr,
    getEnv,
    reader,
    userToIO,
  )
where

import Data.ByteString
import IOSH.Maybe
import IOSH.Protocol
import Pipes hiding (Effect, embed)
import Pipes.Prelude qualified as P
import Polysemy
import System.Environment hiding (getEnv)
import System.Posix.ByteString hiding (getEnv, getEnvironment)
import Prelude hiding (read)

type User :: Effect
data User m a where
  GetEnv :: User m Environment
  Read :: User m (Maybe ByteString)
  Write :: ByteString -> User m ()
  WriteErr :: ByteString -> User m ()

makeSem ''User

reader :: (Member User r) => Producer ByteString (Sem r) ()
reader = P.repeatM read >-> justYielder

userToIO :: (Member (Embed IO) r) => Fd -> Fd -> Fd -> InterpreterFor User r
userToIO i o e = interpret $ \case
  GetEnv -> embed getEnvironment
  Read -> embed $ eofToNothing <$> fdRead i 8192
  (Write str) -> embed . void $ fdWrite o str
  (WriteErr str) -> embed . void $ fdWrite e str
