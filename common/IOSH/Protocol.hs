{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IOSH.Protocol
  ( Handshake (..),
    ClientMessage (..),
    ServerMessage (..),
    Termination (..),
    Environment,
    Args,
    Size,
  )
where

import Data.ByteString
import Data.Int
import Data.Kind
import Data.Serialize
import GHC.Generics
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
  deriving stock (Show, Generic)

type ClientMessage :: Type
data ClientMessage where
  Resize :: Size -> ClientMessage
  Input :: ByteString -> ClientMessage
  deriving stock (Generic)

type ServerMessage :: Type
data ServerMessage where
  Output :: ByteString -> ServerMessage
  Error :: ByteString -> ServerMessage
  deriving stock (Generic)

type Termination :: Type
data Termination where
  Termination :: ExitCode -> Termination
  deriving stock (Generic)

instance Serialize ExitCode

instance Serialize ClientMessage

instance Serialize ServerMessage

instance Serialize Handshake

instance Serialize Termination
