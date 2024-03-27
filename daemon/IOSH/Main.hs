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

aptClientMessageReceiver :: (Member Decoder r, Member Fail r, Member Exit r, Member (Tagged 'Tunnel ByteInput) r, Member (Tagged 'Process ByteOutput) r, Member Resize r) => Bool -> Sem r ()
aptClientMessageReceiver pty = tag @'Process @ByteOutput clientMessageReceiver
  where
    clientMessageReceiver = tag @'Tunnel @ByteInput . runEffect $ for xInputter go
      where
        go (Input str) = lift $ output str
        go (IOSH.Resize size) = lift $ aptResize pty size
        go (ClientTermination code) = lift $ exit code

aptOutputSender :: (Member Race r, Member (Tagged 'Tunnel ByteOutput) r, Member (Tagged 'Process (Tagged 'StandardStream ByteInput)) r, Member (Tagged 'Process (Tagged 'ErrorStream ByteInput)) r, Member (Tagged 'Process ByteInput) r) => Bool -> Sem r ()
aptOutputSender pty =
  tag @'Tunnel @ByteOutput $
    if pty
      then tag @'Process @ByteInput outputSender
      else
        race_
          (tag @'Process @(Tagged 'StandardStream ByteInput) . tag @'StandardStream @ByteInput $ outputSender)
          (tag @'Process @(Tagged 'ErrorStream ByteInput) . tag @'ErrorStream @ByteInput $ outputSender)
  where
    outputSender :: forall r. (Member ByteInput r, Member ByteOutput r) => Sem r ()
    outputSender = runEffect $ inputter >-> P.map Output >-> xOutputter

proveNoneOf :: forall e r a. (Member Fail r) => Sem (e : r) a -> Sem r a
proveNoneOf = interpretH @e (const $ fail "unexpected effect in Sem")

aptExec :: (Member (Scoped PTYParams PTY) r, Member (Scoped ProcessParams Proc.Process) r, Member Fail r) => Handshake -> InterpretersFor (Append PTYEffects ProcessEffects) r
aptExec (Handshake False sessionEnv path args Nothing) go = Proc.exec (InternalProcess sessionEnv path args) . proveNoneOf @ByteOutput . proveNoneOf @ByteInput . proveNoneOf @Resize $ subsume_ go
aptExec (Handshake True sessionEnv path args maybeSize) go = PTY.exec (PTYParams sessionEnv path args maybeSizeOrDefault) . proveNoneOf @(Tagged 'ErrorStream ByteInput) . proveNoneOf @(Tagged 'StandardStream ByteInput) . proveNoneOf @ByteOutput $ subsume_ go
  where
    maybeSizeOrDefault = fromMaybe (80, 24) maybeSize
aptExec (Handshake False _ _ _ (Just _)) _ = fail "cannot apply provided terminal size in non-pty session"

aptResize :: (Member Fail r, Member Resize r) => Bool -> Size -> Sem r ()
aptResize True size = PTY.resize size
aptResize False _ = fail "cannot resize regular process"

sendExitCode :: (Member Wait r, Member ByteOutput r) => Sem r ()
sendExitCode = wait >>= outputX . ServerTermination

runTunnel :: (Member ByteInput r, Member ByteOutput r) => InterpretersFor (Tagged 'Tunnel ByteInput : Tagged 'Tunnel ByteOutput : '[]) r
runTunnel = untagged @'Tunnel @ByteOutput . untagged @'Tunnel @ByteInput

runProcess :: (Member (Scoped PTYParams PTY) r, Member (Scoped ProcessParams Proc.Process) r, Member Fail r) => Handshake -> InterpretersFor (Tagged 'Process ByteOutput : Tagged 'Process ByteInput : Tagged 'Process (Tagged 'StandardStream ByteInput) : Tagged 'Process (Tagged 'ErrorStream ByteInput) : Resize : Wait : '[]) r
runProcess hshake = aptExec hshake . subsume_ . untagPorcess
  where
    untagPorcess :: Sem (Tagged 'Process ByteOutput : Tagged 'Process ByteInput : Tagged 'Process (Tagged 'StandardStream ByteInput) : Tagged 'Process (Tagged 'ErrorStream ByteInput) : r) a -> Sem (ByteInput : ByteOutput : Tagged 'StandardStream ByteInput : Tagged 'ErrorStream ByteInput : r) a
    untagPorcess =
      untagged @'Process @(Tagged 'ErrorStream ByteInput)
        . untagged @'Process @(Tagged 'StandardStream ByteInput)
        . untagged @'Process @ByteInput
        . untagged @'Process @ByteOutput
        . insertAt @4 @(ByteInput : ByteOutput : Tagged 'StandardStream ByteInput : Tagged 'ErrorStream ByteInput : '[])

ioshd ::
  ( Member Fail r,
    Member Race r,
    Member Decoder r,
    Member Exit r,
    Member Async r,
    Member ByteInput r,
    Member ByteOutput r,
    Member (Scoped PTYParams PTY) r,
    Member (Scoped ProcessParams Proc.Process) r
  ) =>
  Sem r ()
ioshd = do
  hshake@(Handshake pty _ _ _ _) <- inputX
  runTunnel . runProcess hshake $ do
    clientMessageReceiverAsync <- async $ aptClientMessageReceiver pty
    result <- race (aptOutputSender pty) (await clientMessageReceiverAsync)
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
