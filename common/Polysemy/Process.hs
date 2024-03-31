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
import IOSH.IO
import IOSH.Maybe
import IOSH.Protocol (Environment)
import Polysemy
import Polysemy.Bundle
import Polysemy.Close
import Polysemy.Input
import Polysemy.Output
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
type ProcessEffects = ByteOutput : Tagged 'StandardStream ByteInput : Tagged 'ErrorStream ByteInput : Wait : Close : '[]

type Process :: Effect
type Process = Bundle ProcessEffects

bundleProcEffects :: (Member Process r) => InterpretersFor ProcessEffects r
bundleProcEffects =
  sendBundle @Close @ProcessEffects
    . sendBundle @Wait @ProcessEffects
    . sendBundle @(Tagged 'ErrorStream ByteInput) @ProcessEffects
    . sendBundle @(Tagged 'StandardStream ByteInput) @ProcessEffects
    . sendBundle @ByteOutput @ProcessEffects

exec :: (Member (Scoped ProcessParams Process) r) => ProcessParams -> InterpretersFor ProcessEffects r
exec params = scoped @_ @Process params . bundleProcEffects . insertAt @5 @'[Process]

scopedProcToIOFinal :: (Member (Final IO) r) => InterpreterFor (Scoped ProcessParams Process) r
scopedProcToIOFinal = embedToFinal @IO . runScopedNew go . raiseUnder
  where
    go param = procParamsToIOFinal param . runBundle

procParamsToIOFinal :: (Member (Final IO) r) => ProcessParams -> InterpretersFor ProcessEffects r
procParamsToIOFinal param sem = resourceToIOFinal $ bracket (openProc param) closeProc (raise . go)
  where
    disableProcBuffering (i, o, e, _) = mapM_ disableBuffering (catMaybes [i, o, e])
    openProc params = embedFinal $ createProcess (toCreateProcess params)
    closeProc hs = embedFinal $ cleanupProcess hs
    toCreateProcess (InternalProcess sessionEnv path args) = (proc path args) {env = sessionEnv, std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
    toCreateProcess (TunnelProcess cmd) = (shell cmd) {std_in = CreatePipe, std_out = CreatePipe}
    go hs = embedToFinal @IO $ embed (disableProcBuffering hs) >> procToIO hs (insertAt @5 @'[Embed IO] sem)

procToIO :: (Member (Embed IO) r) => (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> InterpretersFor ProcessEffects r
procToIO (i, o, e, ph) =
  maybeCloseToIO i
    . waitToIO ph
    . (maybeInputToIO e . untag @'ErrorStream)
    . (maybeInputToIO o . untag @'StandardStream)
    . maybeOutputToIO i

maybeCloseToIO :: (Member (Embed IO) r) => Maybe Handle -> Sem (Close : r) a -> Sem r a
maybeCloseToIO mh = interpret $ \case
  Close -> do
    h <- unmaybeHandle @IO mh
    closeToIO h close

maybeInputToIO :: (Member (Embed IO) r) => Maybe Handle -> InterpreterFor ByteInput r
maybeInputToIO mh = interpret $ \case
  Input -> do
    h <- unmaybeHandle @IO mh
    inputToIO h input

maybeOutputToIO :: (Member (Embed IO) r) => Maybe Handle -> InterpreterFor ByteOutput r
maybeOutputToIO mh = interpret $ \case
  Output str -> do
    h <- unmaybeHandle @IO mh
    outputToIO h $ output str

unmaybeHandle :: forall m r a. (MonadFail m, Member (Embed m) r) => Maybe a -> Sem r a
unmaybeHandle = embed @m . maybeFail "required process stream isn't piped"
