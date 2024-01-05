module IOSH.Async (async_, await_) where

import Control.Concurrent.Async as CA
import Control.Monad
import Polysemy
import Polysemy.Async as PA

async_ :: (Member PA.Async r) => Sem r a -> Sem r ()
async_ = void . PA.async

await_ :: (Member PA.Async r) => CA.Async a -> Sem r ()
await_ = void . await
