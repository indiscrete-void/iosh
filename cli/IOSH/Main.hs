import Control.Monad
import Data.Bool
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

serverMessageReceiver :: (Member Process r, Member Decoder r, Member User r, Member Fail r, Member Exit r) => Sem r ()
serverMessageReceiver = runEffect $ for xReader go
  where
    go (Output str) = lift $ write str
    go (Error str) = lift $ writeErr str
    go (ServerTermination code) = lift $ writeX (ClientTermination code) >> exit code

ttyOutputSender :: (Member Process r, Member User r) => Sem r ()
ttyOutputSender = runEffect $ reader >-> P.map Input >-> xWriter

init :: (Member Process r, Member TTY r) => Bool -> FilePath -> Args -> Sem r () -> Maybe Environment -> Sem r ()
init pty path args go sessionEnv = getSize >>= writeX . Handshake pty sessionEnv path args >> setResizeHandler (writeX . Resize) >> bool rawBracket id pty go

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
