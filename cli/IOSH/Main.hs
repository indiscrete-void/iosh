import Control.Monad
import Data.Bool
import IOSH.Options
import IOSH.Protocol
import IOSH.Protocol qualified as IOSH
import Polysemy hiding (run)
import Polysemy.Async
import Polysemy.Close
import Polysemy.Exit
import Polysemy.Extra.Async
import Polysemy.Extra.Trace
import Polysemy.Fail
import Polysemy.Output
import Polysemy.Output qualified as Sem
import Polysemy.PTY hiding (exec)
import Polysemy.Process
import Polysemy.Serialize
import Polysemy.TTY
import Polysemy.Tagged
import Polysemy.Trace
import Polysemy.Transport
import Polysemy.User
import System.IO
import System.Posix.IO
import System.Process
import Transport.Maybe
import Prelude hiding (init)

serverMessageReceiver :: (Member Exit r, Member (Tagged 'StandardStream ByteOutput) r, Member (Tagged 'ErrorStream ByteOutput) r, Member (InputWithEOF ServerMessage) r, Member (Output ClientMessage) r, Member (Tagged 'StandardStream Close) r, Member (Tagged 'ErrorStream Close) r, Member Trace r) => Sem r ()
serverMessageReceiver = handle go
  where
    go (IOSH.Output str) = tag @'StandardStream @ByteOutput (output str)
    go (Error str) = tag @'ErrorStream @ByteOutput (output str)
    go (ServerEOF StandardStream) = tag @'StandardStream @Close close
    go (ServerEOF ErrorStream) = tag @'ErrorStream @Close close
    go (ServerTermination code) = output (ClientTermination code) >> exit code

ttyOutputSender :: (Member ByteInputWithEOF r, Member (Sem.Output ClientMessage) r) => Sem r ()
ttyOutputSender = transferStream IOSH.Input ClientEOF

init :: forall r. (Member TTY r, Member (Output Handshake) r, Member (Output ClientMessage) r) => Bool -> FilePath -> Args -> Maybe Environment -> Sem r () -> Sem r ()
init pty path args maybeEnv m = do
  maybeSize <- whenMaybe pty getSize
  output $ Handshake pty maybeEnv path args maybeSize
  when pty $ setResizeHandler (output . Resize)
  if pty
    then rawBracket m'
    else m'
  where
    m' = raise_ m

iosh :: (Member Async r, Members User r, Member Fail r, Member Exit r, Member TTY r, Member (Output Handshake) r, Member (InputWithEOF ServerMessage) r, Member (Output ClientMessage) r, Member Trace r) => Bool -> Bool -> String -> [String] -> Sem r ()
iosh pty inheritEnv path args = do
  maybeEnv <- whenMaybe inheritEnv (Just <$> getEnv)
  init pty path args maybeEnv do
    async_ ttyOutputSender
    serverMessageReceiver
  failTermination

main :: IO ()
main = execOptionsParser >>= run
  where
    runUnserialized =
      tag @'StandardStream @ByteInputWithEOF
        . runDecoder
        . deserializeInput @ServerMessage
        . serializeOutput @Handshake
        . serializeOutput @ClientMessage
        . raise3Under @Decoder
        . raise3Under @ByteInputWithEOF
    run (Options pty inheritEnv tunProcCmd path args) =
      runFinal
        . embedToFinal @IO
        . ttyToIOFinal stdInput
        . asyncToIOFinal
        . traceToStderrBuffered
        . scopedProcToIOFinal bufferSize
        . userToIO stdin stdout stderr
        . exitToIO
        . failToEmbed @IO
        . exec (shell tunProcCmd) {std_in = CreatePipe, std_out = CreatePipe}
        . runUnserialized
        $ iosh pty inheritEnv path args
