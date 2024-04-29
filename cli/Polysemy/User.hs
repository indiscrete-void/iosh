module Polysemy.User
  ( User,
    userToIO,
    getEnv,
  )
where

import IOSH.IO
import IOSH.Protocol
import Polysemy
import Polysemy.Tagged
import System.Environment hiding (getEnv)
import System.IO
import Transport.Close
import Transport.Polysemy
import Prelude hiding (read)

type Env :: Effect
data Env m a where
  GetEnv :: Env m Environment

type User :: [Effect]
type User = ByteInputWithEOF ': Tagged 'StandardStream ByteOutput ': Tagged 'ErrorStream ByteOutput ': Tagged 'StandardStream Close ': Tagged 'ErrorStream Close ': Env ': '[]

makeSem ''Env

userToIO :: (Member (Embed IO) r) => Handle -> Handle -> Handle -> InterpretersFor User r
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
