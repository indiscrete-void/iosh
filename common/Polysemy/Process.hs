module Polysemy.Process
  ( ProcessParams (..),
    ProcessEffects,
    Process,
    exec,
    wait,
    scopedProcToIOFinal,
  )
where

import Control.Monad
import Data.Kind
import Data.Maybe
import IOSH.Protocol
import Polysemy
import Polysemy.Bundle
import Polysemy.Resource
import Polysemy.Scoped
import Polysemy.Tagged
import Polysemy.Transport
import Polysemy.Wait
import System.IO
import System.Process
import Prelude hiding (read)

type ProcessParams :: Type
data ProcessParams = InternalProcess (Maybe Environment) FilePath [String] | TunnelProcess String

type ProcessEffects :: [Effect]
type ProcessEffects = ByteOutput : Tagged 'StandardStream ByteInput : Tagged 'ErrorStream ByteInput : Wait : '[]

type Process :: Effect
type Process = Bundle ProcessEffects

bundleProcEffects :: (Member Process r) => InterpretersFor ProcessEffects r
bundleProcEffects =
  sendBundle @Wait @ProcessEffects
    . sendBundle @(Tagged 'ErrorStream ByteInput) @ProcessEffects
    . sendBundle @(Tagged 'StandardStream ByteInput) @ProcessEffects
    . sendBundle @ByteOutput @ProcessEffects

exec :: (Member (Scoped ProcessParams Process) r) => ProcessParams -> InterpretersFor ProcessEffects r
exec params = scoped @_ @Process params . bundleProcEffects . insertAt @4 @'[Process]

scopedProcToIOFinal :: (Member (Final IO) r) => InterpreterFor (Scoped ProcessParams Process) r
scopedProcToIOFinal = embedToFinal @IO . runScopedNew go . raiseUnder
  where
    go param = procParamsToIOFinal param . runBundle

procParamsToIOFinal :: (Member (Final IO) r, Member (Embed IO) r) => ProcessParams -> InterpretersFor ProcessEffects r
procParamsToIOFinal param sem = resourceToIOFinal $ bracket (openProc param) closeProc (raise . maybeProcToIOFinal)
  where
    openProc params = embedFinal $ createProcess (toCreateProcess params)
    closeProc hs = embedFinal $ cleanupProcess hs
    toCreateProcess (InternalProcess sessionEnv path args) = (proc path args) {env = sessionEnv, std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
    toCreateProcess (TunnelProcess cmd) = (shell cmd) {std_in = CreatePipe, std_out = CreatePipe}
    maybeProcToIOFinal (Just i, Just o, Just e, ph) = procToIO i o e ph sem
    maybeProcToIOFinal _ = embedFinal @IO $ fail "required process stream isn't piped"

procToIO :: (Member (Embed IO) r) => Handle -> Handle -> Handle -> ProcessHandle -> InterpretersFor ProcessEffects r
procToIO i o e ph =
  waitToIO ph
    . (inputToIO e . untag @'ErrorStream)
    . (inputToIO i . untag @'StandardStream)
    . outputToIO o
