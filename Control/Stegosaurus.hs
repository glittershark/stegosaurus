module Control.Stegosaurus where

import Prelude

throwIfNothing :: (MonadThrow m, Exception e) => e -> Maybe a -> m a
throwIfNothing _ (Just v) = return v
throwIfNothing ex Nothing = throwM ex
