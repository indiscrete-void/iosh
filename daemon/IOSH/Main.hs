import Control.Monad
import Data.Either
import IOSH.Protocol
import Pipes hiding (embed)
import Pipes.Prelude qualified as P
import Polysemy
import Polysemy.Conc
import Polysemy.Fail
import Polysemy.PTY hiding (Resize)
import Polysemy.PTY.IO
import Polysemy.Serialize
import Polysemy.State
import Polysemy.Transport
import Polysemy.Transport.IO
import System.IO

clientMessageReceiver :: (Member ByteInput r, Member (PTY h) r, Member (State CarriedOverByteString) r) => h -> Sem r ()
clientMessageReceiver h = runEffect $ for xInputter go
  where
    go (Stdin str) = lift $ write h str
    go (Resize wh) = lift $ resize h wh

ptyOutputSender :: (Member ByteOutput r, Member (PTY h) r) => h -> Sem r ()
ptyOutputSender h = runEffect $ reader h >-> P.map Stdout >-> xOutputter

iosh :: forall h r. (Member ByteInput r, Member ByteOutput r, Member Fail r, Member (PTY h) r, Member Race r) => Sem r ()
iosh = runDecoder $ do
  (Handshake procPath procArgs size) <- inputX
  h <- exec @h procPath procArgs size
  result <- race (ptyOutputSender h) (clientMessageReceiver h)
  close h
  when (isLeft result) $ wait h >>= outputX . Termination

main :: IO ()
main = do
  mapM_ (`hSetBuffering` NoBuffering) [stdin, stdout]
  runFinal
    . (interpretRace . embedToFinal @IO)
    . ptyToIO
    . inputToIO stdin
    . outputToIO stdout
    . failToEmbed @IO
    $ iosh @PTYHandle
