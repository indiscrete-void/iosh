{-# OPTIONS_GHC -Wno-orphans #-}

module IOSH.Protocol
  ( Handshake (..),
    ClientMessage (..),
    ServerMessage (..),
    failTermination,
    bufferSize,
  )
where

import Data.ByteString
import Data.Kind
import Data.Serialize
import GHC.Generics
import Polysemy
import Polysemy.Fail
import Polysemy.PTY
import Polysemy.Process
import System.Exit

bufferSize :: Int
bufferSize = 8192

type Handshake :: Type
data Handshake where
  Handshake :: Bool -> Maybe Environment -> FilePath -> Args -> Maybe Size -> Handshake
  deriving stock (Generic, Show)

type ClientMessage :: Type
data ClientMessage where
  Resize :: Size -> ClientMessage
  Input :: ByteString -> ClientMessage
  ClientEOF :: ClientMessage
  ClientTermination :: ExitCode -> ClientMessage
  deriving stock (Generic, Show)

type ServerMessage :: Type
data ServerMessage where
  Output :: ByteString -> ServerMessage
  Error :: ByteString -> ServerMessage
  ServerEOF :: StreamKind -> ServerMessage
  ServerTermination :: ExitCode -> ServerMessage
  deriving stock (Generic, Show)

failTermination :: (Member Fail r) => Sem r a
failTermination = fail "session ended before termination procedure was done"

instance Serialize StreamKind

instance Serialize ExitCode

instance Serialize ClientMessage

instance Serialize ServerMessage

instance Serialize Handshake
