import Control.Monad
import Data.Either
import Data.Maybe
import IOSH.IO
import IOSH.Protocol
import IOSH.Protocol qualified as IOSH
import Polysemy hiding (run)
import Polysemy.Async
import Polysemy.Async_
import Polysemy.Close
import Polysemy.Conc hiding (Scoped)
import Polysemy.Exit
import Polysemy.Fail
import Polysemy.Internal hiding (run)
import Polysemy.Output
import Polysemy.Output qualified as Sem
import Polysemy.PTY (PTY, PTYEffects, PTYParams (..), Resize, scopedPTYToIOFinal)
import Polysemy.PTY qualified as PTY
import Polysemy.Process (ProcessEffects, scopedProcToIOFinal)
import Polysemy.Process qualified as Proc
import Polysemy.Scoped
import Polysemy.Serialize
import Polysemy.Tagged
import Polysemy.Transport
import Polysemy.Wait
import System.IO
import System.Process

clientMessageReceiver :: (Member Fail r, Member Exit r, Member (InputWithEOF ClientMessage) r, Member ByteOutput r, Member Resize r, Member Close r) => Bool -> Sem r ()
clientMessageReceiver pty = handle go
  where
    go (IOSH.Input str) = output str
    go (IOSH.Resize size) = resize pty size
    go (ClientTermination code) = exit code
    go ClientEOF = close

outputSender :: (Member Race r, Member (Sem.Output ServerMessage) r, Member (Tagged 'StandardStream ByteInputWithEOF) r, Member (Tagged 'ErrorStream ByteInputWithEOF) r, Member ByteInputWithEOF r) => Bool -> Sem r ()
outputSender pty =
  if pty
    then transferStream IOSH.Output (ServerEOF StandardStream)
    else
      race_
        (tag @'StandardStream @ByteInputWithEOF $ transferStream IOSH.Output (ServerEOF StandardStream))
        (tag @'ErrorStream @ByteInputWithEOF $ transferStream Error (ServerEOF ErrorStream))

proveNo :: forall e r a. (Member Fail r) => Sem (e ': r) a -> Sem r a
proveNo = interpretH @e (const $ fail "unexpected effect in Sem")

exec :: forall r a. (Member (Scoped PTYParams PTY) r, Member (Scoped CreateProcess Proc.Process) r, Member Fail r) => Handshake -> Sem (Append PTYEffects (Append ProcessEffects r)) a -> Sem r a
exec hshake m = case hshake of
  (Handshake False sessionEnv path args Nothing) -> go . proveNo @ByteInputWithEOF . proveNo @Resize $ m'
    where
      go = Proc.exec (proc path args) {env = sessionEnv, std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
  (Handshake True sessionEnv path args maybeSize) -> go . proveNo @(Tagged 'ErrorStream ByteInputWithEOF) . proveNo @(Tagged 'StandardStream ByteInputWithEOF) $ m'
    where
      size = fromMaybe (80, 24) maybeSize
      go = PTY.exec (PTYParams sessionEnv path args size)
  (Handshake False _ _ _ (Just _)) -> fail "cannot apply provided terminal size in non-pty session"
  where
    m' :: (Members (Append PTYEffects ProcessEffects) r', Subsume r r') => Sem r' a
    m' = subsume_ m

resize :: (Member Fail r, Member Resize r) => Bool -> Size -> Sem r ()
resize True size = PTY.resize size
resize False _ = fail "cannot resize regular process"

sendExitCode :: (Member Wait r, Member (Output ServerMessage) r) => Sem r ()
sendExitCode = wait >>= output . ServerTermination

ioshd :: (Member Fail r, Member Race r, Member Exit r, Member Async r, Member (Scoped PTYParams PTY) r, Member (Scoped CreateProcess Proc.Process) r, Member (InputWithEOF Handshake) r, Member (InputWithEOF ClientMessage) r, Member (Output ServerMessage) r) => Sem r ()
ioshd = do
  hshake@(Handshake pty _ _ _ _) <- inputOrFail
  exec hshake do
    clientMessageReceiverAsync <- async $ clientMessageReceiver pty
    result <- race (outputSender pty) (await clientMessageReceiverAsync)
    when (isLeft result) $ sendExitCode >> await_ clientMessageReceiverAsync
  failTermination

main :: IO ()
main = mapM_ disableBuffering [stdin, stdout] >> run
  where
    runUnserialized =
      runDecoder
        . deserializeInput @ClientMessage
        . deserializeInput @Handshake
        . serializeOutput @ServerMessage
        . raise3Under @Decoder
    run =
      runFinal
        . interpretRace
        . asyncToIOFinal
        . embedToFinal @IO
        . scopedProcToIOFinal
        . scopedPTYToIOFinal
        . inputToIO bufferSize stdin
        . outputToIO stdout
        . exitToIO
        . failToEmbed @IO
        . runUnserialized
        $ ioshd
