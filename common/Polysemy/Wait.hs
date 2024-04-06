module Polysemy.Wait (Wait (..), wait, waitToIO) where

import Polysemy
import System.Exit
import System.Process

type Wait :: Effect
data Wait m a where
  Wait :: Wait m ExitCode

makeSem ''Wait

waitToIO :: (Member (Embed IO) r) => ProcessHandle -> InterpreterFor Wait r
waitToIO ph = interpret \case
  Wait -> embed $ waitForProcess ph
