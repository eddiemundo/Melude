module Melude.Monad
  ( module Base
  , (<<)
  ) where


import Control.Monad as Base (Monad((>>), (>>=)), mapM, mapM_, forM, forM_, sequence, sequence_, (=<<), (>=>), (<=<), join, liftM, liftM2, liftM3, liftM4, liftM5, ap)

(<<) :: Monad m => m a -> m b -> m a
(<<) lma lmb = do
  a <- lma
  _ <- lmb
  pure a
