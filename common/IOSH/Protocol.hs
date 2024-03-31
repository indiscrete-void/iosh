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
    failEOF,
  )
where

import Data.ByteString
import Data.Int
import Data.Kind
import Data.Serialize
import GHC.Generics
import Polysemy
import Polysemy.Fail
import System.Exit

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
  EOF :: ClientMessage
  ClientTermination :: ExitCode -> ClientMessage
  deriving stock (Generic)

type ServerMessage :: Type
data ServerMessage where
  Output :: ByteString -> ServerMessage
  Error :: ByteString -> ServerMessage
  ServerTermination :: ExitCode -> ServerMessage
  deriving stock (Generic)

failTermination :: (Member Fail r) => Sem r a
failTermination = fail "session ended before termination procedure was done"

failEOF :: (Member Fail r) => Sem r a
failEOF = fail "end of pipe reached"

instance Serialize ExitCode

instance Serialize ClientMessage

instance Serialize ServerMessage

instance Serialize Handshake
