module Polysemy.TTY
  ( TTY (..),
    getSize,
    setResizeHandler,
    attributeBracket,
    setRaw,
  )
where

import Data.Kind
import IOSH.Protocol
import Polysemy
import Prelude hiding (read)

type TTY :: (Type -> Type) -> Type -> Type
data TTY m a where
  GetSize :: TTY m Size
  SetResizeHandler :: (Size -> m ()) -> TTY m ()
  AttributeBracket :: m a -> TTY m a
  SetRaw :: TTY m ()

makeSem ''TTY
