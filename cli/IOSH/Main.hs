import Control.Monad
import Data.Bool
import IOSH.Options
import IOSH.Protocol
import Pipes hiding (await)
import Pipes.Prelude qualified as P
import Polysemy hiding (run)
import Polysemy.Async
import Polysemy.Fail
import Polysemy.Process hiding (reader, write)
import Polysemy.Scoped
import Polysemy.Serialize
import Polysemy.TTY
import Polysemy.User
import System.IO
import System.Posix.IO
import Prelude hiding (init)

async_ :: (Member Async r) => Sem r a -> Sem r ()
async_ = void . async

rawBracket :: (Member TTY r) => Sem r a -> Sem r a
rawBracket m = attributeBracket $ setRawAttributes >> m

serverMessageReceiver :: (Member Process r, Member Decoder r, Member User r, Member Fail r) => Sem r ()
serverMessageReceiver = runEffect $ for xReader go
  where
    go (Output str) = lift $ write str
    go (Error str) = lift $ writeErr str
    go (Termination code) = lift $ exit code

ttyOutputSender :: (Member Process r, Member User r) => Sem r ()
ttyOutputSender = runEffect $ reader >-> P.map Input >-> xWriter

ptyIOSH :: (Member Process r, Member Decoder r, Member TTY r, Member Async r, Member User r, Member Fail r) => Maybe Environment -> FilePath -> Args -> Sem r ()
ptyIOSH maybeEnv path args = rawBracket $ do
  getSize >>= writeX . Handshake True maybeEnv path args
  setResizeHandler (writeX . Resize)
  async_ ttyOutputSender
  serverMessageReceiver

procIOSH :: (Member Process r, Member Decoder r, Member Async r, Member User r, Member Fail r) => Maybe Environment -> FilePath -> Args -> Sem r ()
procIOSH maybeEnv path args = do
  writeX $ Handshake False maybeEnv path args Nothing
  async_ ttyOutputSender
  serverMessageReceiver

iosh :: (Member (Scoped ProcessParams Process) r, Member Async r, Member TTY r, Member User r, Member Decoder r, Member Fail r) => Options -> Sem r ()
iosh (Options pty inheritEnv tunProcCmd path args) =
  exec (TunnelProcess tunProcCmd) $ do
    maybeEnv <- bool (pure Nothing) (Just <$> getEnv) inheritEnv
    if pty
      then ptyIOSH maybeEnv path args
      else procIOSH maybeEnv path args

main :: IO ()
main = execOptionsParser >>= run
  where
    run =
      runFinal
        . ttyToIOFinal stdInput
        . asyncToIOFinal
        . scopedProcToIOFinal
        . embedToFinal @IO
        . userToIO stdInput stdOutput stdError
        . failToEmbed @IO
        . runDecoder
        . iosh
