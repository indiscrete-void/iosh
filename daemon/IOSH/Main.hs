import Control.Concurrent.Async qualified as Control.Concurrent
import Control.Monad
import Data.Either
import Data.Maybe
import IOSH.IO
import IOSH.Protocol
import Pipes hiding (await)
import Pipes.Prelude qualified as P
import Polysemy hiding (run)
import Polysemy.Async
import Polysemy.Conc hiding (Scoped)
import Polysemy.Exit
import Polysemy.Fail
import Polysemy.PTY (PTY, PTYParams (..), scopedPTYToIOFinal)
import Polysemy.PTY qualified as PTY
import Polysemy.Process (Process, ProcessParams (..), scopedProcToIOFinal)
import Polysemy.Process qualified as Proc
import Polysemy.Scoped
import Polysemy.Serialize
import Polysemy.Transport
import System.IO

await_ :: (Member Async r) => Control.Concurrent.Async a -> Sem r ()
await_ = void . await

procClientMessageReceiver :: (Member ByteInput r, Member Process r, Member Decoder r, Member Fail r, Member Exit r) => Sem r ()
procClientMessageReceiver = runEffect $ for xInputter go
  where
    go (Input str) = lift $ Proc.write str
    go (Resize _) = lift $ fail "cannot resize regular process"
    go (ClientTermination code) = lift $ exit code

procOutputSender :: (Member ByteOutput r, Member Process r) => Sem r ()
procOutputSender = runEffect $ Proc.reader >-> P.map Output >-> xOutputter

procErrorSender :: (Member ByteOutput r, Member Process r) => Sem r ()
procErrorSender = runEffect $ Proc.errReader >-> P.map Error >-> xOutputter

procIOSHD :: (Member ByteInput r, Member ByteOutput r, Member Race r, Member (Scoped ProcessParams Process) r, Member Decoder r, Member Fail r, Member Exit r, Member Async r) => Maybe Environment -> FilePath -> Args -> Sem r ()
procIOSHD sessionEnv path args =
  Proc.exec (InternalProcess sessionEnv path args) $ do
    procClientMessageReceiverAsync <- async procClientMessageReceiver
    result <- race (race procOutputSender procErrorSender) (await procClientMessageReceiverAsync)
    when (isLeft result) $ (Proc.wait >>= outputX . ServerTermination) >> await_ procClientMessageReceiverAsync

ptyClientMessageReceiver :: (Member ByteInput r, Member PTY r, Member Decoder r, Member Fail r, Member Exit r) => Sem r ()
ptyClientMessageReceiver = runEffect $ for xInputter go
  where
    go (Input str) = lift $ PTY.write str
    go (Resize wh) = lift $ PTY.resize wh
    go (ClientTermination code) = lift $ exit code

ptyOutputSender :: (Member ByteOutput r, Member PTY r) => Sem r ()
ptyOutputSender = runEffect $ PTY.reader >-> P.map Output >-> xOutputter

ptyIOSHD :: (Member ByteInput r, Member ByteOutput r, Member Race r, Member (Scoped PTYParams PTY) r, Member Decoder r, Member Fail r, Member Exit r, Member Async r) => Maybe Environment -> FilePath -> Args -> Maybe Size -> Sem r ()
ptyIOSHD sessionEnv path args maybeSize =
  let size = fromMaybe (0, 0) maybeSize
   in PTY.exec (PTYParams sessionEnv path args size) $ do
        ptyClientMessageReceiverAsync <- async ptyClientMessageReceiver
        result <- race ptyOutputSender (await ptyClientMessageReceiverAsync)
        when (isLeft result) $ (PTY.wait >>= outputX . ServerTermination) >> await_ ptyClientMessageReceiverAsync

ioshd :: (Member ByteInput r, Member ByteOutput r, Member Fail r, Member Race r, Member (Scoped PTYParams PTY) r, Member (Scoped ProcessParams Process) r, Member Decoder r, Member Exit r, Member Async r) => Sem r ()
ioshd = do
  (Handshake pty sessionEnv path args maybeSize) <- inputX
  if pty
    then ptyIOSHD sessionEnv path args maybeSize
    else procIOSHD sessionEnv path args
  failTermination

main :: IO ()
main = mapM_ disableBuffering [stdin, stdout] >> run
  where
    run =
      runFinal
        . interpretRace
        . asyncToIOFinal
        . embedToFinal @IO
        . scopedPTYToIOFinal
        . scopedProcToIOFinal
        . inputToIO stdin
        . outputToIO stdout
        . exitToIO
        . failToEmbed @IO
        . runDecoder
        $ ioshd
