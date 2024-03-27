import Control.Monad
import Data.Either
import Data.Kind
import Data.Maybe
import IOSH.IO
import IOSH.Protocol
import IOSH.Protocol qualified as IOSH
import Pipes hiding (await)
import Pipes.Prelude qualified as P
import Polysemy hiding (run)
import Polysemy.Async
import Polysemy.Async_
import Polysemy.Conc hiding (Scoped)
import Polysemy.Exit
import Polysemy.Fail
import Polysemy.Internal.Kind
import Polysemy.Output hiding (Output)
import Polysemy.PTY (PTY, PTYEffects, PTYParams (..), Resize, scopedPTYToIOFinal)
import Polysemy.PTY qualified as PTY
import Polysemy.Process (ProcessEffects, ProcessParams (..), scopedProcToIOFinal)
import Polysemy.Process qualified as Proc
import Polysemy.Scoped
import Polysemy.Serialize
import Polysemy.Tagged
import Polysemy.Transport
import Polysemy.Untag
import Polysemy.Wait
import System.IO

type Stream :: Type
data Stream = Tunnel | Process

clientMessageReceiver :: (Member Decoder r, Member Fail r, Member Exit r, Member (Tagged 'Tunnel ByteInput) r, Member (Tagged 'Process ByteOutput) r, Member Resize r) => Bool -> Sem r ()
clientMessageReceiver pty = tag @'Process @ByteOutput go
  where
    go = tag @'Tunnel @ByteInput . runEffect $ for xInputter handle
      where
        handle (Input str) = lift $ output str
        handle (IOSH.Resize size) = lift $ resize pty size
        handle (ClientTermination code) = lift $ exit code

outputSender :: (Member Race r, Member (Tagged 'Tunnel ByteOutput) r, Member (Tagged 'Process (Tagged 'StandardStream ByteInput)) r, Member (Tagged 'Process (Tagged 'ErrorStream ByteInput)) r, Member (Tagged 'Process ByteInput) r) => Bool -> Sem r ()
outputSender pty =
  tag @'Tunnel @ByteOutput $
    if pty
      then tag @'Process @ByteInput go
      else
        race_
          (tag @'Process @(Tagged 'StandardStream ByteInput) . tag @'StandardStream @ByteInput $ go)
          (tag @'Process @(Tagged 'ErrorStream ByteInput) . tag @'ErrorStream @ByteInput $ go)
  where
    go :: forall r. (Member ByteInput r, Member ByteOutput r) => Sem r ()
    go = runEffect $ inputter >-> P.map Output >-> xOutputter

proveNo :: forall e r a. (Member Fail r) => Sem (e : r) a -> Sem r a
proveNo = interpretH @e (const $ fail "unexpected effect in Sem")

exec :: (Member (Scoped PTYParams PTY) r, Member (Scoped ProcessParams Proc.Process) r, Member Fail r) => Handshake -> InterpretersFor (Append PTYEffects ProcessEffects) r
exec (Handshake False sessionEnv path args Nothing) m = go . proveNo @ByteOutput . proveNo @ByteInput . proveNo @Resize $ subsume_ m
  where
    go = Proc.exec (InternalProcess sessionEnv path args)
exec (Handshake True sessionEnv path args maybeSize) m = go . proveNo @(Tagged 'ErrorStream ByteInput) . proveNo @(Tagged 'StandardStream ByteInput) . proveNo @ByteOutput $ subsume_ m
  where
    maybeSizeOrDefault = fromMaybe (80, 24) maybeSize
    go = PTY.exec (PTYParams sessionEnv path args maybeSizeOrDefault)
exec (Handshake False _ _ _ (Just _)) _ = fail "cannot apply provided terminal size in non-pty session"

resize :: (Member Fail r, Member Resize r) => Bool -> Size -> Sem r ()
resize True size = PTY.resize size
resize False _ = fail "cannot resize regular process"

sendExitCode :: (Member Wait r, Member ByteOutput r) => Sem r ()
sendExitCode = wait >>= outputX . ServerTermination

runTunnel :: (Member ByteInput r, Member ByteOutput r) => InterpretersFor (Tagged 'Tunnel ByteInput : Tagged 'Tunnel ByteOutput : '[]) r
runTunnel = untagged @'Tunnel @ByteOutput . untagged @'Tunnel @ByteInput

runProcess :: (Member (Scoped PTYParams PTY) r, Member (Scoped ProcessParams Proc.Process) r, Member Fail r) => Handshake -> InterpretersFor (Tagged 'Process ByteOutput : Tagged 'Process ByteInput : Tagged 'Process (Tagged 'StandardStream ByteInput) : Tagged 'Process (Tagged 'ErrorStream ByteInput) : Resize : Wait : '[]) r
runProcess hshake = exec hshake . subsume_ . untagPorcess
  where
    untagPorcess :: Sem (Tagged 'Process ByteOutput : Tagged 'Process ByteInput : Tagged 'Process (Tagged 'StandardStream ByteInput) : Tagged 'Process (Tagged 'ErrorStream ByteInput) : r) a -> Sem (ByteInput : ByteOutput : Tagged 'StandardStream ByteInput : Tagged 'ErrorStream ByteInput : r) a
    untagPorcess =
      untagged @'Process @(Tagged 'ErrorStream ByteInput)
        . untagged @'Process @(Tagged 'StandardStream ByteInput)
        . untagged @'Process @ByteInput
        . untagged @'Process @ByteOutput
        . insertAt @4 @(ByteInput : ByteOutput : Tagged 'StandardStream ByteInput : Tagged 'ErrorStream ByteInput : '[])

ioshd :: (Member Fail r, Member Race r, Member Decoder r, Member Exit r, Member Async r, Member ByteInput r, Member ByteOutput r, Member (Scoped PTYParams PTY) r, Member (Scoped ProcessParams Proc.Process) r) => Sem r ()
ioshd = do
  hshake@(Handshake pty _ _ _ _) <- inputX
  runTunnel . runProcess hshake $ do
    clientMessageReceiverAsync <- async $ clientMessageReceiver pty
    result <- race (outputSender pty) (await clientMessageReceiverAsync)
    when (isLeft result) $ sendExitCode >> await_ clientMessageReceiverAsync
  failTermination

main :: IO ()
main = mapM_ disableBuffering [stdin, stdout] >> run
  where
    run =
      runFinal
        . interpretRace
        . asyncToIOFinal
        . embedToFinal @IO
        . scopedProcToIOFinal
        . scopedPTYToIOFinal
        . inputToIO stdin
        . outputToIO stdout
        . exitToIO
        . failToEmbed @IO
        . runDecoder
        $ ioshd
