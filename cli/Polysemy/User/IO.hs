module Polysemy.User.IO
  ( userToIO,
  )
where

import Control.Monad
import Polysemy
import Polysemy.Transport.IO
import Polysemy.User
import System.Exit
import System.Posix.ByteString

userToIO :: (Member (Embed IO) r) => Fd -> Fd -> InterpreterFor User r
userToIO i o = interpret $ \case
  IsTerminal -> embed $ queryTerminal i
  Read -> embed $ eofToNothing <$> fdRead i 8192
  (Write str) -> embed . void $ fdWrite o str
  (Exit code) -> embed $ exitWith code
