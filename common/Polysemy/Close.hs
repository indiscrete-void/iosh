module Polysemy.Close
  ( Close (..),
    close,
    closeToIO,
  )
where

import Polysemy
import System.IO

type Close :: Effect
data Close m a where
  Close :: Close m ()

makeSem ''Close

closeToIO :: (Member (Embed IO) r) => Handle -> InterpreterFor Close r
closeToIO h = interpret $ \case
  Close -> embed $ hClose h
