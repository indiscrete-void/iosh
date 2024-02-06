module IOSH.Process
  ( ProcessHandles,
    openProcess,
    closeProcess,
  )
where

import Data.Kind
import System.IO
import System.Process

type ProcessHandles :: Type
type ProcessHandles = (Handle, Handle, Handle, ProcessHandle)

openProcess :: CreateProcess -> IO ProcessHandles
openProcess params = do
  (Just i, Just o, Just e, ph) <- createProcess (params {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe})
  mapM_ (`hSetBuffering` NoBuffering) [i, o, e]
  pure (i, o, e, ph)

closeProcess :: ProcessHandles -> IO ()
closeProcess (i, o, e, ph) = cleanupProcess (Just i, Just o, Just e, ph)
