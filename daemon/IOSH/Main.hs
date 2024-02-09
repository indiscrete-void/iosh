import Control.Monad
import Data.Either
import Data.Maybe
import IOSH.IO
import IOSH.Protocol
import Pipes
import Pipes.Prelude qualified as P
import Polysemy hiding (run)
import Polysemy.Conc hiding (Scoped)
import Polysemy.Fail
import Polysemy.PTY (PTY, PTYParams (..), scopedPTYToIOFinal)
import Polysemy.PTY qualified as PTY
import Polysemy.Process (Process, ProcessParams (..), scopedProcToIOFinal)
import Polysemy.Process qualified as Proc
import Polysemy.Scoped
import Polysemy.Serialize
import Polysemy.Transport
import System.IO

procClientMessageReceiver :: (Member ByteInput r, Member Process r, Member Decoder r, Member Fail r) => Sem r ()
procClientMessageReceiver = runEffect $ for xInputter go
  where
    go (Input str) = lift $ Proc.write str
    go (Resize _) = lift $ fail "cannot resize regular process"

procOutputSender :: (Member ByteOutput r, Member Process r) => Sem r ()
procOutputSender = runEffect $ Proc.reader >-> P.map Output >-> xOutputter

procErrorSender :: (Member ByteOutput r, Member Process r) => Sem r ()
procErrorSender = runEffect $ Proc.errReader >-> P.map Error >-> xOutputter

procIOSHD :: (Member ByteInput r, Member ByteOutput r, Member Race r, Member (Scoped ProcessParams Process) r, Member Decoder r, Member Fail r) => Maybe Environment -> FilePath -> Args -> Sem r ()
procIOSHD maybeEnv path args =
  Proc.exec (InternalProcess maybeEnv path args) $ do
    result <- race (race procOutputSender procErrorSender) procClientMessageReceiver
    when (isLeft result) $ Proc.wait >>= outputX . Termination

ptyClientMessageReceiver :: (Member ByteInput r, Member PTY r, Member Decoder r, Member Fail r) => Sem r ()
ptyClientMessageReceiver = runEffect $ for xInputter go
  where
    go (Input str) = lift $ PTY.write str
    go (Resize wh) = lift $ PTY.resize wh

ptyOutputSender :: (Member ByteOutput r, Member PTY r) => Sem r ()
ptyOutputSender = runEffect $ PTY.reader >-> P.map Output >-> xOutputter

ptyIOSHD :: (Member ByteInput r, Member ByteOutput r, Member Race r, Member (Scoped PTYParams PTY) r, Member Decoder r, Member Fail r) => Maybe Environment -> FilePath -> Args -> Maybe Size -> Sem r ()
ptyIOSHD maybeEnv path args maybeSize =
  let size = fromMaybe (0, 0) maybeSize
   in PTY.exec (PTYParams maybeEnv path args size) $ do
        result <- race ptyOutputSender ptyClientMessageReceiver
        when (isLeft result) $ PTY.wait >>= outputX . Termination

ioshd :: (Member ByteInput r, Member ByteOutput r, Member Fail r, Member Race r, Member (Scoped PTYParams PTY) r, Member (Scoped ProcessParams Process) r, Member Decoder r) => Sem r ()
ioshd = do
  (Handshake pty maybeEnv path args maybeSize) <- inputX
  if pty
    then ptyIOSHD maybeEnv path args maybeSize
    else procIOSHD maybeEnv path args

main :: IO ()
main = mapM_ disableBuffering [stdin, stdout] >> run
  where
    run =
      runFinal
        . interpretRace
        . embedToFinal @IO
        . scopedPTYToIOFinal
        . scopedProcToIOFinal
        . inputToIO stdin
        . outputToIO stdout
        . failToEmbed @IO
        . runDecoder
        $ ioshd
