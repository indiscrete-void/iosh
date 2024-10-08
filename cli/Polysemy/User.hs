module Polysemy.User
  ( User,
    userToIO,
    getEnv,
  )
where

import IOSH.Protocol
import Polysemy
import Polysemy.Close
import Polysemy.PTY
import Polysemy.Process (StreamKind (..))
import Polysemy.Tagged
import Polysemy.Trace
import Polysemy.Transport
import System.Environment hiding (getEnv)
import System.IO
import Prelude hiding (read)

type Env :: Effect
data Env m a where
  GetEnv :: Env m Environment

type User :: [Effect]
type User = ByteInputWithEOF ': Tagged 'StandardStream ByteOutput ': Tagged 'ErrorStream ByteOutput ': Tagged 'StandardStream Close ': Tagged 'ErrorStream Close ': Env ': '[]

makeSem ''Env

userToIO :: (Member (Embed IO) r, Member Trace r) => Handle -> Handle -> Handle -> InterpretersFor User r
userToIO i o e =
  envToIO
    . (closeToIO e . untag @'ErrorStream)
    . (closeToIO o . untag @'StandardStream)
    . (outputToIO e . untag @'ErrorStream)
    . (outputToIO o . untag @'StandardStream)
    . inputToIO bufferSize i

envToIO :: (Member (Embed IO) r) => InterpreterFor Env r
envToIO = interpret \case
  GetEnv -> embed getEnvironment
