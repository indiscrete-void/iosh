module Polysemy.Untag (untagged) where

import Polysemy
import Polysemy.Tagged

untagged :: forall k e r a. (Member e r) => Sem (Tagged k e ': r) a -> Sem r a
untagged = subsume . untag
