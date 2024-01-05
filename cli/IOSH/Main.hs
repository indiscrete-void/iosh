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
import System.Process
import Prelude hiding (error)

serverMessageReceiver :: (Member ByteInput r, Member TTY r, Member (State CarriedOverByteString) r) => Sem r ()
serverMessageReceiver = runEffect $ for xInputter go
  where
    go (Stdout str) = lift $ write str
    go (Stderr str) = lift $ error str
    go (Termination code) = lift $ exit code

ttyOutputSender :: (Member ByteOutput r, Member TTY r) => Sem r ()
ttyOutputSender = runEffect $ reader >-> P.map Stdin >-> xOutputter

iosh :: (Member ByteInput r, Member ByteOutput r, Member Async r, Member TTY r) => FilePath -> Args -> Sem r ()
iosh path args =
  evalState @CarriedOverByteString Nothing $
    getSize >>= outputX . Handshake path args >> setSizeChH (outputX . Resize) >> async ttyOutputSender >> (async serverMessageReceiver >>= void . await)

main :: IO ()
main = do
  (Options tunProcCmd execPath execArgs) <- execOptionsParser
  (Just tunIn, Just tunOut, _, _) <- createProcess (shell tunProcCmd) {std_in = CreatePipe, std_out = CreatePipe}
  mapM_ (`hSetBuffering` NoBuffering) [tunIn, tunOut, stdin, stdout]
  runFinal
    . (ttyToIOFinal stdin stdout stderr . embedToFinal @IO)
    . (asyncToIOFinal . embedToFinal @IO)
    . inputToIO tunOut
    . outputToIO tunIn
    . failToEmbed @IO
    $ iosh execPath execArgs
