import Control.Monad
import Data.Bool
import Debug.Trace
import IOSH.Options
import IOSH.Protocol
import Pipes hiding (await)
import Pipes.Prelude qualified as P
import Polysemy hiding (run)
import Polysemy.Async
import Polysemy.Exit
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

ttyOutputSender :: (Member Process r, Member User r) => Sem r ()
ttyOutputSender = runEffect $ reader >-> P.map Input >-> xWriter

ptyInit :: (Member Process r, Member TTY r) => FilePath -> Args -> Maybe Environment -> Sem r ()
ptyInit path args sessionEnv = rawBracket $ do
  getSize >>= writeX . Handshake True sessionEnv path args
  setResizeHandler (writeX . Resize)

procInit :: (Member Process r) => FilePath -> Args -> Maybe Environment -> Sem r ()
procInit path args sesionEnv = do
  writeX $ Handshake False sesionEnv path args Nothing

init :: (Member Process r, Member TTY r) => Bool -> FilePath -> Args -> Maybe Environment -> Sem r ()
init = bool procInit ptyInit

getSessionEnv :: (Member User r) => Bool -> Sem r (Maybe Environment)
getSessionEnv = bool (pure Nothing) (Just <$> getEnv)

exitGracefully :: (Member Exit r, Member Decoder r, Member Fail r, Member Process r) => Sem r ()
exitGracefully = do
  msg@(Termination code) <- readX
  writeX msg
  exit code

iosh :: (Member (Scoped ProcessParams Process) r, Member Async r, Member TTY r, Member User r, Member Decoder r, Member Fail r, Member Exit r) => Options -> Sem r ()
iosh (Options pty inheritEnv tunProcCmd path args) =
  exec (TunnelProcess tunProcCmd) $ do
    getSessionEnv inheritEnv >>= init pty path args
    async_ ttyOutputSender
    serverMessageReceiver
    exitGracefully

main :: IO ()
main = execOptionsParser >>= run
  where
    run =
      runFinal
        . ttyToIOFinal stdInput
        . asyncToIOFinal
        . scopedProcToIOFinal
        . embedToFinal @IO
        . exitToIO
        . userToIO stdInput stdOutput stdError
        . failToEmbed @IO
        . runDecoder
        . iosh
