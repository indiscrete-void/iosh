{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IOSH.Protocol
  ( Handshake (..),
    ClientMessage (..),
    ServerMessage (..),
    Environment,
    Args,
    Size,
    failTermination,
    transferStream,
    StreamKind (..),
  )
where

import Data.ByteString
import Data.Int
import Data.Kind
import Data.Serialize
import GHC.Generics
import Pipes
import Pipes.Prelude qualified as P
import Polysemy
import Polysemy.Fail
import Polysemy.Output
import Polysemy.Transport
import System.Exit

type StreamKind :: Type
data StreamKind = StandardStream | ErrorStream
  deriving stock (Generic)

type Environment :: Type
type Environment = [(String, String)]

type Args :: Type
type Args = [String]

type Size :: Type
type Size = (Int16, Int16)

type Handshake :: Type
data Handshake where
  Handshake :: Bool -> Maybe Environment -> FilePath -> Args -> Maybe Size -> Handshake
  deriving stock (Generic)

type ClientMessage :: Type
data ClientMessage where
  Resize :: Size -> ClientMessage
  Input :: ByteString -> ClientMessage
  ClientEOF :: ClientMessage
  ClientTermination :: ExitCode -> ClientMessage
  deriving stock (Generic)

type ServerMessage :: Type
data ServerMessage where
  Output :: ByteString -> ServerMessage
  Error :: ByteString -> ServerMessage
  ServerEOF :: StreamKind -> ServerMessage
  ServerTermination :: ExitCode -> ServerMessage
  deriving stock (Generic)

failTermination :: (Member Fail r) => Sem r a
failTermination = fail "session ended before termination procedure was done"

transferStream :: (Member (InputWithEOF a) r, Member (Output msg) r, Member (Output eofMsg) r) => (a -> msg) -> eofMsg -> Sem r ()
transferStream f eof = runEffect (inputter >-> P.map f >-> outputter) >> output eof

instance Serialize StreamKind

instance Serialize ExitCode

instance Serialize ClientMessage

instance Serialize ServerMessage

instance Serialize Handshake
