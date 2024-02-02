import Control.Exception
import Data.Bool
import IOSH.Options
import IOSH.Protocol
import Pipes hiding (await)
import Pipes.Prelude qualified as P
import Polysemy hiding (run)
import Polysemy.Async
import Polysemy.Fail
import Polysemy.Serialize
import Polysemy.TTY
import Polysemy.Transport
import Polysemy.User
import System.IO
import System.Posix.IO
import System.Process
import Prelude hiding (init)

async_ :: (Member Async r) => Sem r a -> Sem r ()
async_ = void . async

rawBracket :: (Member TTY r) => Sem r a -> Sem r a
rawBracket m = attributeBracket $ setRawAttributes >> m

serverMessageReceiver :: (Member ByteInput r, Member Decoder r, Member User r) => Sem r ()
serverMessageReceiver = runEffect $ for xInputter go
  where
    go (Output str) = lift $ write str
    go (Error str) = lift $ writeErr str
    go (Termination code) = lift $ exit code

ttyOutputSender :: (Member ByteOutput r, Member User r) => Sem r ()
ttyOutputSender = runEffect $ reader >-> P.map Input >-> xOutputter

ptyIOSH :: (Member ByteInput r, Member ByteOutput r, Member Decoder r, Member TTY r, Member Async r, Member User r) => FilePath -> Args -> Sem r ()
ptyIOSH path args = rawBracket $ do
  getSize >>= outputX . Handshake path args . Just
  setResizeHandler (outputX . Resize)
  async_ ttyOutputSender
  serverMessageReceiver

procIOSH :: (Member ByteInput r, Member ByteOutput r, Member Decoder r, Member Async r, Member User r) => FilePath -> Args -> Sem r ()
procIOSH path args = do
  outputX $ Handshake path args Nothing
  async_ ttyOutputSender
  serverMessageReceiver

iosh :: (Member ByteInput r, Member ByteOutput r, Member Async r, Member TTY r, Member User r, Member Decoder r) => Bool -> FilePath -> Args -> Sem r ()
iosh True = ptyIOSH
iosh False = procIOSH

run :: Bool -> FilePath -> Args -> Handle -> Handle -> IO ()
run interactive execPath execArgs tunIn tunOut =
  runFinal
    . (ttyToIOFinal stdInput . embedToFinal @IO)
    . (asyncToIOFinal . embedToFinal @IO)
    . userToIO stdInput stdOutput stdError
    . inputToIO tunOut
    . outputToIO tunIn
    . failToEmbed @IO
    . runDecoder
    $ iosh interactive execPath execArgs

main :: IO ()
main = do
  (Options interactive tunProcCmd execPath execArgs) <- execOptionsParser
  hs@(Just tunIn, Just tunOut, _, _) <- createProcess (shell tunProcCmd) {std_in = CreatePipe, std_out = CreatePipe}
  finally (mapM_ (`hSetBuffering` NoBuffering) [tunIn, tunOut] >> run interactive execPath execArgs tunIn tunOut) (cleanupProcess hs)
