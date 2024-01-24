module IOSH.Maybe (maybeFail) where

maybeFail :: (MonadFail m) => String -> Maybe a -> m a
maybeFail str = maybe (fail str) pure
