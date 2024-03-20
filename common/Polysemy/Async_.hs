module Polysemy.Async_ (async_, await_) where

import Control.Concurrent.Async qualified as Control.Concurrent
import Control.Monad
import Polysemy
import Polysemy.Async

await_ :: (Member Async r) => Control.Concurrent.Async a -> Sem r ()
await_ = void . await

async_ :: (Member Async r) => Sem r a -> Sem r ()
async_ = void . async
