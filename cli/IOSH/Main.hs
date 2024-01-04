import IOSH.Options hiding (execArgs, execPath, tunProcCmd)
import IOSH.Protocol
import Pipes
import Pipes.Prelude qualified as P
import Polysemy
import Polysemy.Conc
import Polysemy.Fail
import Polysemy.Serialize
import Polysemy.State
import Polysemy.TTY
import Polysemy.TTY.IO
import Polysemy.Transport
import Polysemy.Transport.IO
import System.IO
import System.Process

rawTTYBracket :: (Member TTY r) => Sem r a -> Sem r a
rawTTYBracket m = setEcho False *> m <* setEcho True

serverMessageReceiver :: (Member ByteInput r, Member TTY r, Member (State CarriedOverByteString) r) => Sem r ()
serverMessageReceiver = runEffect $ for xInputter go
  where
    go (PTYData str) = lift $ write str
    go (Termination code) = lift $ exit code

ttyOutputSender :: (Member ByteOutput r, Member TTY r) => Sem r ()
ttyOutputSender = runEffect $ reader >-> P.map TTYData >-> xOutputter

iosh :: (Member ByteInput r, Member ByteOutput r, Member Race r, Member TTY r) => FilePath -> Args -> Sem r ()
iosh path args = evalState @CarriedOverByteString Nothing $
  rawTTYBracket $ do
    getSize >>= outputX . Handshake path args
    setSizeChH (outputX . Resize)
    race_ serverMessageReceiver ttyOutputSender

main :: IO ()
main = do
  (Options tunProcCmd execPath execArgs) <- execOptionsParser
  (Just tunIn, Just tunOut, _, _) <- createProcess (shell tunProcCmd) {std_in = CreatePipe, std_out = CreatePipe}
  mapM_ (`hSetBuffering` NoBuffering) [tunIn, tunOut, stdin, stdout]
  runFinal
    . (ttyToIOFinal stdin stdout . embedToFinal @IO)
    . (interpretRace . embedToFinal @IO)
    . inputToIO tunOut
    . outputToIO tunIn
    . failToEmbed @IO
    $ iosh execPath execArgs
