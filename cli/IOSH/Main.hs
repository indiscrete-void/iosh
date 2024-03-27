import Control.Monad
import Data.Bool
import Data.Kind
import IOSH.Maybe
import IOSH.Options
import IOSH.Protocol
import Pipes hiding (await)
import Pipes.Prelude qualified as P
import Polysemy hiding (run)
import Polysemy.Async
import Polysemy.Async_
import Polysemy.Exit
import Polysemy.Fail
import Polysemy.Output hiding (Output)
import Polysemy.Process
import Polysemy.Scoped
import Polysemy.Serialize
import Polysemy.TTY
import Polysemy.Tagged
import Polysemy.Transport
import Polysemy.Untag
import Polysemy.User
import Polysemy.Wait
import System.IO
import System.Posix.IO
import Prelude hiding (init)

type Stream :: Type
data Stream = User | Tunnel

serverMessageReceiver :: (Member Decoder r, Member Fail r, Member Exit r, Member (Tagged 'User (Tagged 'StandardStream ByteOutput)) r, Member (Tagged 'User (Tagged 'ErrorStream ByteOutput)) r, Member (Tagged 'Tunnel ByteInput) r, Member (Tagged 'Tunnel ByteOutput) r) => Sem r ()
serverMessageReceiver = tag @'Tunnel @ByteInput . tag @'Tunnel @ByteOutput . tag @'User @(Tagged 'StandardStream ByteOutput) . tag @'User @(Tagged 'ErrorStream ByteOutput) . runEffect $ for xInputter go
  where
    go (Output str) = lift $ tag @'StandardStream @ByteOutput (output str)
    go (Error str) = lift $ tag @'ErrorStream @ByteOutput (output str)
    go (ServerTermination code) = lift $ outputX (ClientTermination code) >> exit code

ttyOutputSender :: (Member (Tagged 'User ByteInput) r, Member (Tagged 'Tunnel ByteOutput) r) => Sem r ()
ttyOutputSender = tag @'User @ByteInput . tag @'Tunnel @ByteOutput . runEffect $ inputter >-> P.map Input >-> xOutputter

init :: forall r. (Member TTY r, Member (Tagged 'Tunnel ByteOutput) r) => Bool -> FilePath -> Args -> Maybe Environment -> Sem r () -> Sem r ()
init pty path args maybeEnv go = tag @'Tunnel @ByteOutput $ do
  maybeSize <- whenMaybe pty getSize
  outputX $ Handshake pty maybeEnv path args maybeSize
  when pty $ setResizeHandler (outputX . Resize)
  rawBracket (raise go)

runUser :: (Member (Tagged 'StandardStream ByteOutput) r, Member (Tagged 'ErrorStream ByteOutput) r, Member ByteInput r) => InterpretersFor (Tagged 'User ByteInput : Tagged 'User (Tagged 'ErrorStream ByteOutput) : Tagged 'User (Tagged 'StandardStream ByteOutput) : '[]) r
runUser = untagged @'User @(Tagged 'StandardStream ByteOutput) . untagged @'User @(Tagged 'ErrorStream ByteOutput) . untagged @'User @ByteInput

runTunnel :: forall r. (Member (Scoped ProcessParams Process) r) => String -> InterpretersFor (Tagged 'Tunnel ByteInput : Tagged 'Tunnel ByteOutput : Wait : '[]) r
runTunnel tunProcCmd = exec (TunnelProcess tunProcCmd) . untag @'Tunnel @ByteOutput . retag @'Tunnel @'StandardStream @ByteInput . raise2Under @(Tagged 'StandardStream ByteInput) . raise2Under @(Tagged 'ErrorStream ByteInput)

iosh :: (Member Async r, Members User r, Member Decoder r, Member Fail r, Member Exit r, Member TTY r, Member (Scoped ProcessParams Process) r) => Options -> Sem r ()
iosh (Options pty inheritEnv tunProcCmd path args) = runUser . runTunnel tunProcCmd $ do
  maybeEnv <- whenMaybe inheritEnv (Just <$> getEnv)
  init pty path args maybeEnv $ do
    async_ ttyOutputSender
    serverMessageReceiver
  failTermination

main :: IO ()
main = execOptionsParser >>= run
  where
    run =
      runFinal
        . ttyToIOFinal stdInput
        . asyncToIOFinal
        . scopedProcToIOFinal
        . embedToFinal @IO
        . userToIO stdin stdout stderr
        . exitToIO
        . failToEmbed @IO
        . runDecoder
        . iosh
