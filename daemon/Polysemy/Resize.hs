module Polysemy.Resize
  ( Resize,
    resize,
    resizeToIO,
    ps2s,
  )
where

import Control.Monad
import Data.Bifunctor
import IOSH.Protocol hiding (Resize)
import Polysemy
import System.Posix.Pty

type Resize :: Effect
data Resize m a where
  Resize :: Size -> Resize m ()

makeSem ''Resize

ps2s :: Size -> (Int, Int)
ps2s = join bimap fromIntegral

resizeToIO :: (Member (Embed IO) r) => Pty -> InterpreterFor Resize r
resizeToIO pty = interpret \case
  Resize size -> embed $ resizePty pty (ps2s size)
