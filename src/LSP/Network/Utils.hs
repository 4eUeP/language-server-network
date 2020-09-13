module LSP.Network.Utils
  ( untilM_
  , toMaybe
  ) where

-- | Run action until prediction is false.
untilM_ :: Monad m => m a -> (a -> m Bool) -> m ()
untilM_ m p = go
  where
    go = do
      r <- m >>= p
      if r then go else pure ()

toMaybe :: Bool -> a -> Maybe a
toMaybe p x = if p then Just x else Nothing
{-# INLINE toMaybe #-}
