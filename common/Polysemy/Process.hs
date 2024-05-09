module Polysemy.Process
  ( ProcessEffects,
    Process,
    exec,
    wait,
    scopedProcToIOFinal,
  )
where

import Control.Monad
import Data.Maybe
import IOSH.IO
import IOSH.Protocol (StreamKind (..))
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
import Transport.Maybe
import Prelude hiding (read)

type ProcessEffects :: [Effect]
type ProcessEffects = ByteOutput ': Tagged 'StandardStream ByteInputWithEOF ': Tagged 'ErrorStream ByteInputWithEOF ': Wait ': Close ': '[]

type Process :: Effect
type Process = Bundle ProcessEffects

bundleProcEffects :: (Member Process r) => InterpretersFor ProcessEffects r
bundleProcEffects =
  sendBundle @Close @ProcessEffects
    . sendBundle @Wait @ProcessEffects
    . sendBundle @(Tagged 'ErrorStream ByteInputWithEOF) @ProcessEffects
    . sendBundle @(Tagged 'StandardStream ByteInputWithEOF) @ProcessEffects
    . sendBundle @ByteOutput @ProcessEffects

exec :: (Member (Scoped p Process) r) => p -> InterpretersFor ProcessEffects r
exec params = scoped @_ @Process params . bundleProcEffects . insertAt @5 @'[Process]

scopedProcToIOFinal :: (Member (Final IO) r) => InterpreterFor (Scoped CreateProcess Process) r
scopedProcToIOFinal = embedToFinal @IO . runScopedBundle procParamsToIOFinal . raiseUnder

procParamsToIOFinal :: (Member (Final IO) r) => CreateProcess -> InterpretersFor ProcessEffects r
procParamsToIOFinal param sem = resourceToIOFinal $ bracket (openProc param) closeProc (raise . go)
  where
    disableProcBuffering (i, o, e, _) = mapM_ disableBuffering (catMaybes [i, o, e])
    openProc params = embedFinal $ createProcess params
    closeProc hs = embedFinal $ cleanupProcess hs
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
    . (inputToIO bufferSize o . untag @'StandardStream)
    . outputToIO i

maybeInputToIO :: (Member (Embed IO) r) => Maybe Handle -> InterpreterFor ByteInputWithEOF r
maybeInputToIO mh = interpret \case
  Input -> do
    h <- unmaybeHandle @IO mh
    inputToIO bufferSize h input

unmaybeHandle :: forall m r a. (MonadFail m, Member (Embed m) r) => Maybe a -> Sem r a
unmaybeHandle = embed @m . maybeFail "required process stream isn't piped"
