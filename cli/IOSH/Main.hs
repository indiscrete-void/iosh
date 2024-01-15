import IOSH.Async
import IOSH.Options hiding (execArgs, execPath, tunProcCmd)
import IOSH.Protocol
import Pipes hiding (await)
import Pipes.Prelude qualified as P
import Polysemy
import Polysemy.Async
import Polysemy.Fail
import Polysemy.Serialize
import Polysemy.State
import Polysemy.TTY
import Polysemy.TTY.IO
import Polysemy.Transport
import Polysemy.Transport.IO
import System.IO
import System.Posix.IO
import System.Process
import Prelude

rawBracket :: (Member TTY r) => Sem r a -> Sem r a
rawBracket m = attributeBracket $ setRaw >> m

serverMessageReceiver :: (Member ByteInput r, Member TTY r, Member (State CarriedOverByteString) r) => Sem r ()
serverMessageReceiver = runEffect $ for xInputter go
  where
    go (Stdout str) = lift $ write str
    go (Termination code) = lift $ exit code

ttyOutputSender :: (Member ByteOutput r, Member TTY r) => Sem r ()
ttyOutputSender = runEffect $ reader >-> P.map Stdin >-> xOutputter

iosh :: (Member ByteInput r, Member ByteOutput r, Member Async r, Member TTY r) => FilePath -> Args -> Sem r ()
iosh path args = evalState @CarriedOverByteString Nothing . rawBracket $ do
  getSize >>= outputX . Handshake path args
  setResizeHandler (outputX . Resize)
  async_ ttyOutputSender
  serverMessageReceiver

main :: IO ()
main = do
  (Options tunProcCmd execPath execArgs) <- execOptionsParser
  (Just tunIn, Just tunOut, _, _) <- createProcess (shell tunProcCmd) {std_in = CreatePipe, std_out = CreatePipe}
  mapM_ (`hSetBuffering` NoBuffering) [tunIn, tunOut, stdin, stdout]
  runFinal
    . (ttyToIOFinal stdInput stdOutput . embedToFinal @IO)
    . (asyncToIOFinal . embedToFinal @IO)
    . inputToIO tunOut
    . outputToIO tunIn
    . failToEmbed @IO
    $ iosh execPath execArgs
