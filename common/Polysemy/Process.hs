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
import IOSH.Protocol (Environment, StreamKind (..))
import Polysemy
import Polysemy.Bundle
import Polysemy.Close
import Polysemy.Input
import Polysemy.Resource
import Polysemy.Scoped
import Polysemy.ScopedBundle
import Polysemy.Tagged
import Polysemy.Transport
import Polysemy.Wait
import System.IO
import System.Process
import Prelude hiding (read)

type ProcessParams :: Type
data ProcessParams = InternalProcess (Maybe Environment) FilePath [String] | TunnelProcess String

type ProcessEffects :: [Effect]
type ProcessEffects = ByteOutput : Tagged 'StandardStream ByteInputWithEOF : Tagged 'ErrorStream ByteInputWithEOF : Wait : Close : '[]

type Process :: Effect
type Process = Bundle ProcessEffects

bundleProcEffects :: (Member Process r) => InterpretersFor ProcessEffects r
bundleProcEffects =
  sendBundle @Close @ProcessEffects
    . sendBundle @Wait @ProcessEffects
    . sendBundle @(Tagged 'ErrorStream ByteInputWithEOF) @ProcessEffects
    . sendBundle @(Tagged 'StandardStream ByteInputWithEOF) @ProcessEffects
    . sendBundle @ByteOutput @ProcessEffects

exec :: (Member (Scoped ProcessParams Process) r) => ProcessParams -> InterpretersFor ProcessEffects r
exec params = scoped @_ @Process params . bundleProcEffects . insertAt @5 @'[Process]

scopedProcToIOFinal :: (Member (Final IO) r) => InterpreterFor (Scoped ProcessParams Process) r
scopedProcToIOFinal = embedToFinal @IO . runScopedBundle procParamsToIOFinal . raiseUnder

procParamsToIOFinal :: (Member (Final IO) r) => ProcessParams -> InterpretersFor ProcessEffects r
procParamsToIOFinal param sem = resourceToIOFinal $ bracket (openProc param) closeProc (raise . go)
  where
    disableProcBuffering (i, o, e, _) = mapM_ disableBuffering (catMaybes [i, o, e])
    openProc params = embedFinal $ createProcess (toCreateProcess params)
    closeProc hs = embedFinal $ cleanupProcess hs
    toCreateProcess (InternalProcess sessionEnv path args) = (proc path args) {env = sessionEnv, std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
    toCreateProcess (TunnelProcess cmd) = (shell cmd) {std_in = CreatePipe, std_out = CreatePipe}
    go hs = embedToFinal @IO $ embed (disableProcBuffering hs) >> unmaybeHandles hs >>= flip procToIO (insertAt @5 @'[Embed IO] sem)
    unmaybeHandles (mi, mo, me, ph) = do
      i <- unmaybeHandle @IO mi
      o <- unmaybeHandle @IO mo
      return (i, o, me, ph)

procToIO :: (Member (Embed IO) r) => (Handle, Handle, Maybe Handle, ProcessHandle) -> InterpretersFor ProcessEffects r
procToIO (i, o, e, ph) =
  closeToIO i
    . waitToIO ph
    . (maybeInputToIO e . untag @'ErrorStream)
    . (inputToIO o . untag @'StandardStream)
    . outputToIO i

maybeInputToIO :: (Member (Embed IO) r) => Maybe Handle -> InterpreterFor ByteInputWithEOF r
maybeInputToIO mh = interpret $ \case
  Input -> do
    h <- unmaybeHandle @IO mh
    inputToIO h input

unmaybeHandle :: forall m r a. (MonadFail m, Member (Embed m) r) => Maybe a -> Sem r a
unmaybeHandle = embed @m . maybeFail "required process stream isn't piped"
