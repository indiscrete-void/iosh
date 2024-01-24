import Data.Bool
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
import Polysemy.Transport
import Polysemy.User
import System.IO
import System.Posix.IO
import System.Process
import Prelude hiding (init)

rawBracket :: (Member TTY r) => Sem r a -> Sem r a
rawBracket m = attributeBracket $ setRaw >> m

serverMessageReceiver :: (Member ByteInput r, Member (State CarriedOverByteString) r, Member User r) => Sem r ()
serverMessageReceiver = runEffect $ for xInputter go
  where
    go (Output str) = lift $ write str
    go (Error str) = lift $ writeErr str
    go (Termination code) = lift $ exit code

ttyOutputSender :: (Member ByteOutput r, Member User r) => Sem r ()
ttyOutputSender = runEffect $ reader >-> P.map Input >-> xOutputter

ptyIOSH :: (Member ByteInput r, Member ByteOutput r, Member (State CarriedOverByteString) r, Member TTY r, Member Async r, Member User r) => FilePath -> Args -> Sem r ()
ptyIOSH path args = rawBracket $ do
  getSize >>= outputX . Handshake path args . Just
  setResizeHandler (outputX . Resize)
  async_ ttyOutputSender
  serverMessageReceiver

procIOSH :: (Member ByteInput r, Member ByteOutput r, Member (State CarriedOverByteString) r, Member Async r, Member User r) => FilePath -> Args -> Sem r ()
procIOSH path args = do
  outputX $ Handshake path args Nothing
  async_ ttyOutputSender
  serverMessageReceiver

iosh :: (Member ByteInput r, Member ByteOutput r, Member Async r, Member TTY r, Member User r) => FilePath -> Args -> Sem r ()
iosh path args = runDecoder $ isTerminal >>= bool (ptyIOSH path args) (procIOSH path args)

main :: IO ()
main = do
  (Options tunProcCmd execPath execArgs) <- execOptionsParser
  (Just tunIn, Just tunOut, _, _) <- createProcess (shell tunProcCmd) {std_in = CreatePipe, std_out = CreatePipe}
  mapM_ (`hSetBuffering` NoBuffering) [tunIn, tunOut, stdin, stdout]
  runFinal
    . (ttyToIOFinal stdInput . embedToFinal @IO)
    . (asyncToIOFinal . embedToFinal @IO)
    . userToIO stdInput stdOutput stdError
    . inputToIO tunOut
    . outputToIO tunIn
    . failToEmbed @IO
    $ iosh execPath execArgs
