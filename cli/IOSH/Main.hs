import Control.Monad
import Data.Bool
import IOSH.Options
import IOSH.Protocol
import Pipes hiding (await)
import Pipes.Prelude qualified as P
import Polysemy hiding (run)
import Polysemy.Async
import Polysemy.Async_
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

serverMessageReceiver :: (Member Process r, Member Decoder r, Member User r, Member Fail r, Member Exit r) => Sem r ()
serverMessageReceiver = runEffect $ for xReader go
  where
    go (Output str) = lift $ write str
    go (Error str) = lift $ writeErr str
    go (ServerTermination code) = lift $ writeX (ClientTermination code) >> exit code

ttyOutputSender :: (Member Process r, Member User r) => Sem r ()
ttyOutputSender = runEffect $ reader >-> P.map Input >-> xWriter

ptyInit :: (Member Process r, Member TTY r) => FilePath -> Args -> Sem r () -> Maybe Environment -> Sem r ()
ptyInit path args go sessionEnv = getSize >>= writeX . Handshake True sessionEnv path args >> setResizeHandler (writeX . Resize) >> rawBracket go

procInit :: (Member Process r) => FilePath -> Args -> Sem r () -> Maybe Environment -> Sem r ()
procInit path args go sesionEnv = writeX (Handshake False sesionEnv path args Nothing) >> go

init :: (Member Process r, Member TTY r) => Bool -> FilePath -> Args -> Sem r () -> Maybe Environment -> Sem r ()
init = bool procInit ptyInit

getSessionEnv :: (Member User r) => Bool -> Sem r (Maybe Environment)
getSessionEnv = bool (pure Nothing) (Just <$> getEnv)

iosh :: (Member (Scoped ProcessParams Process) r, Member Async r, Member TTY r, Member User r, Member Decoder r, Member Fail r, Member Exit r) => Options -> Sem r ()
iosh (Options pty inheritEnv tunProcCmd path args) = exec (TunnelProcess tunProcCmd) $ getSessionEnv inheritEnv >>= init pty path args (async_ ttyOutputSender >> serverMessageReceiver) >> failTermination

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
