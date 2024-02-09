module Polysemy.Exit
  ( Exit (..),
    exit,
    exitToIO,
  )
where

import Polysemy
import System.Exit

type Exit :: Effect
data Exit m a where
  Exit :: ExitCode -> Exit m () -- exit >> m = exit

makeSem ''Exit

exitToIO :: (Member (Embed IO) r) => InterpreterFor Exit r
exitToIO = interpret $ \case
  (Exit code) -> embed $ exitWith code
