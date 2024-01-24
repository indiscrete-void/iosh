import Control.Monad
import Data.Either
import IOSH.Protocol
import Pipes hiding (embed)
import Pipes.Prelude qualified as P
import Polysemy
import Polysemy.Conc hiding (Scoped)
import Polysemy.Fail
import Polysemy.PTY hiding (Resize)
import Polysemy.Scoped
import Polysemy.Serialize
import Polysemy.State
import Polysemy.Transport
import System.IO

clientMessageReceiver :: (Member ByteInput r, Member PTY r, Member (State CarriedOverByteString) r) => Sem r ()
clientMessageReceiver = runEffect $ for xInputter go
  where
    go (Input str) = lift $ write str
    go (Resize wh) = lift $ resize wh

ptyOutputSender :: (Member ByteOutput r, Member PTY r) => Sem r ()
ptyOutputSender = runEffect $ reader >-> P.map Output >-> xOutputter

iosh :: (Member ByteInput r, Member ByteOutput r, Member Fail r, Member Race r, Member (Scoped PTYParams PTY) r) => Sem r ()
iosh = runDecoder $ do
  (Handshake procPath procArgs size) <- inputX
  exec (PTYParams procPath procArgs size) $ do
    result <- race ptyOutputSender clientMessageReceiver
    when (isLeft result) $ wait >>= outputX . Termination

main :: IO ()
main = do
  mapM_ (`hSetBuffering` NoBuffering) [stdin, stdout]
  runFinal
    . (interpretRace . embedToFinal @IO)
    . scopedPTYToIO
    . inputToIO stdin
    . outputToIO stdout
    . failToEmbed @IO
    $ iosh
