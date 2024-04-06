module Polysemy.ScopedBundle (runScopedBundle) where

import Polysemy
import Polysemy.Bundle
import Polysemy.Internal.Sing
import Polysemy.Opaque
import Polysemy.Scoped

runScopedBundle :: (KnownList r') => (forall q. param -> InterpretersFor r' (Opaque q : r)) -> Sem (Scoped param (Bundle r') : r) a -> Sem r a
runScopedBundle f = runScopedNew \param -> f param . runBundle
