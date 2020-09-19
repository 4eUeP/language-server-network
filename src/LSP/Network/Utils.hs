module LSP.Network.Utils
  ( untilM_
  , retry
  , toMaybe
  , textShow
  ) where

import           Control.Concurrent     (threadDelay)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text              as Text
import           Numeric.Natural        (Natural)

-- | Run action until prediction is false.
untilM_ :: Monad m => m a -> (a -> m Bool) -> m ()
untilM_ m p = go
  where
    go = do
      r <- m >>= p
      if r then go else pure ()

retry :: MonadIO m => m a -> (a -> Bool) -> Natural -> Int -> m (Maybe a)
retry m cond maxn delay
  | maxn <= 0 = pure Nothing
  | otherwise = m >>= \r -> if cond r then pure (Just r)
                                      else do liftIO $ threadDelay delay
                                              retry m cond (maxn - 1) delay

toMaybe :: Bool -> a -> Maybe a
toMaybe p x = if p then Just x else Nothing
{-# INLINE toMaybe #-}

textShow :: Show a => a -> Text.Text
textShow = Text.pack . show
{-# INLINE textShow #-}
