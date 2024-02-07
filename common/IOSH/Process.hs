module IOSH.Process
  ( ProcessHandles,
    ioHandles,
    openProcess,
    closeProcess,
    disableProcessBuffering,
  )
where

import Data.Kind
import IOSH.IO
import System.IO
import System.Process

type ProcessHandles :: Type
type ProcessHandles = (Handle, Handle, Handle, ProcessHandle)

ioHandles :: ProcessHandles -> [Handle]
ioHandles (i, o, e, _) = [i, o, e]

openProcess :: CreateProcess -> IO ProcessHandles
openProcess params = do
  (Just i, Just o, Just e, ph) <- createProcess (params {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe})
  pure (i, o, e, ph)

disableProcessBuffering :: ProcessHandles -> IO ()
disableProcessBuffering = mapM_ disableBuffering . ioHandles

closeProcess :: ProcessHandles -> IO ()
closeProcess (i, o, e, ph) = cleanupProcess (Just i, Just o, Just e, ph)
