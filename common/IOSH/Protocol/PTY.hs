{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IOSH.Protocol.PTY
  ( Handshake (..),
    ClientMessage (..),
    ServerMessage (..),
    Args,
    Size,
  )
where

import Data.ByteString
import Data.Kind
import Data.Serialize
import GHC.Generics
import IOSH.Protocol (Args, Size)
import System.Exit

type Handshake :: Type
data Handshake where
  Handshake :: FilePath -> Args -> Size -> Handshake
  deriving stock (Generic)

type ClientMessage :: Type
data ClientMessage where
  Resize :: Size -> ClientMessage
  Stdin :: ByteString -> ClientMessage
  deriving stock (Generic)

type ServerMessage :: Type
data ServerMessage where
  Stdout :: ByteString -> ServerMessage
  Termination :: ExitCode -> ServerMessage
  deriving stock (Generic)

instance Serialize ClientMessage

instance Serialize ServerMessage

instance Serialize Handshake
