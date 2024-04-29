module Polysemy.PTY
  ( PTY,
    PTYEffects,
    Resize,
    PTYParams (..),
    exec,
    resize,
    scopedPTYToIOFinal,
  )
where

import Control.Monad
import Data.Bifunctor
import Data.Kind
import IOSH.Protocol hiding (Input, Output, Resize)
import Polysemy
import Polysemy.Bundle
import Polysemy.Close
import Polysemy.Input
import Polysemy.Output
import Polysemy.Resource
import Polysemy.Scoped
import Polysemy.ScopedBundle
import Polysemy.Transport
import Polysemy.Wait
import System.IO
import System.Posix.Pty
import System.Process
import Transport.Maybe
import Prelude hiding (read)

type PTYParams :: Type
data PTYParams = PTYParams (Maybe Environment) FilePath Args Size

type Resize :: Effect
data Resize m a where
  Resize :: Size -> Resize m ()

type PTYEffects :: [Effect]
type PTYEffects = Resize ': ByteInputWithEOF ': ByteOutput ': Wait ': Close ': '[]

type PTY :: Effect
type PTY = Bundle PTYEffects

makeSem ''Resize

bundlePTYEffects :: (Member PTY r) => InterpretersFor PTYEffects r
bundlePTYEffects =
  sendBundle @Close @PTYEffects
    . sendBundle @Wait @PTYEffects
    . sendBundle @ByteOutput @PTYEffects
    . sendBundle @ByteInputWithEOF @PTYEffects
    . sendBundle @Resize @PTYEffects

exec :: (Member (Scoped PTYParams PTY) r) => PTYParams -> InterpretersFor PTYEffects r
exec params = scoped @_ @PTY params . bundlePTYEffects . insertAt @5 @'[PTY]

ps2s :: Size -> (Int, Int)
ps2s = join bimap fromIntegral

scopedPTYToIOFinal :: (Member (Final IO) r) => InterpreterFor (Scoped PTYParams PTY) r
scopedPTYToIOFinal = runScopedBundle ptyParamsToIOFinal

ptyParamsToIOFinal :: (Member (Final IO) r) => PTYParams -> InterpretersFor PTYEffects r
ptyParamsToIOFinal param sem = resourceToIOFinal $ bracket (embedOpenPty param) embedClosePty (raise . go)
  where
    embedOpenPty (PTYParams sessionEnv path args size) = embedFinal $ spawnWithPty sessionEnv True path args (ps2s size)
    embedClosePty (pty, _) = embedFinal $ closePty pty
    go = embedToFinal @IO . flip (uncurry ptyToIO) (insertAt @5 @'[Embed IO] sem)

ptyToIO :: (Member (Embed IO) r) => Pty -> ProcessHandle -> InterpretersFor PTYEffects r
ptyToIO pty ph =
  closeToPTYIO pty
    . waitToIO ph
    . outputToPtyIO pty
    . inputToPtyIO pty
    . resizeToIO pty

closeToPTYIO :: (Member (Embed IO) r) => Pty -> InterpreterFor Close r
closeToPTYIO pty = interpret \case
  Close -> embed $ closePty pty

inputToPtyIO :: (Member (Embed IO) r) => Pty -> InterpreterFor ByteInputWithEOF r
inputToPtyIO pty = interpret \case
  Input -> embed $ threadWaitReadPty pty >> ioErrorToNothing (readPty pty)

outputToPtyIO :: (Member (Embed IO) r) => Pty -> InterpreterFor ByteOutput r
outputToPtyIO pty = interpret \case
  Output str -> embed $ threadWaitWritePty pty >> writePty pty str

resizeToIO :: (Member (Embed IO) r) => Pty -> InterpreterFor Resize r
resizeToIO pty = interpret \case
  Resize size -> embed $ resizePty pty (ps2s size)
