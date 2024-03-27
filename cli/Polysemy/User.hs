module Polysemy.User
  ( User,
    userToIO,
    getEnv,
  )
where

import IOSH.Protocol
import Polysemy
import Polysemy.Tagged
import Polysemy.Transport
import System.Environment hiding (getEnv)
import System.IO
import Prelude hiding (read)

type Env :: Effect
data Env m a where
  GetEnv :: Env m Environment

type User :: [Effect]
type User = ByteInput : Tagged 'StandardStream ByteOutput : Tagged 'ErrorStream ByteOutput : Env : '[]

makeSem ''Env

userToIO :: (Member (Embed IO) r) => Handle -> Handle -> Handle -> InterpretersFor User r
userToIO i o e = envToIO . outputToIO e . untag @'ErrorStream . outputToIO o . untag @'StandardStream . inputToIO i

envToIO :: (Member (Embed IO) r) => InterpreterFor Env r
envToIO = interpret $ \case
  GetEnv -> embed getEnvironment
