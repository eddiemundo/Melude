{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
module Melude.ValidateT where

import Prelude hiding (error, or)
import Control.Monad.State.Lazy as Lazy
import Control.Monad.State.Strict as Strict
import GHC.Stack (HasCallStack, callStack)
import Control.Applicative (Applicative(liftA2))
import Data.Function ((&))
import Data.Functor ((<&>))
import Control.Monad.Reader (MonadReader(ask, local), ReaderT)
import qualified Melude.ValidateT.Internal as Internal
import Data.Sequence (Seq)
import Control.Category ((>>>))
import Control.Monad.Trans.Control (MonadTransControl (liftWith, restoreT))
import Control.Monad.Identity (IdentityT)
import Control.Monad.Base (MonadBase(liftBase))
import qualified Data.Sequence as Seq
import Data.Functor.Identity (Identity(Identity))
import qualified Data.Sequence.NonEmpty as NonEmptySeq



type NonEmptySeq a = Internal.NonEmptySeq a

-- Notes:
-- If we want MonadBaseControl then write a MonadTransControl instance.
newtype ValidateT e m a = ValidateT { runValidateT :: m (Internal.Result e a) }
  deriving Functor

type Validate e a = ValidateT e Identity a

instance Applicative m => Applicative (ValidateT e m) where
  pure a = a & Internal.Success & pure & ValidateT
  (<*>) (ValidateT mrf) (ValidateT mra) = ValidateT $ liftA2 (<*>) mrf mra

instance Monad m => Monad (ValidateT e m) where
  (>>=) (ValidateT mr) f = ValidateT $ mr >>= \case
    Internal.Success a -> f a & runValidateT
    Internal.Failures errors -> Internal.Failures errors & pure



instance MonadTrans (ValidateT e) where
  lift :: Functor m => m a -> ValidateT e m a
  lift ma = ma <&> pure & ValidateT

instance MonadIO m => MonadIO (ValidateT e m) where
  liftIO = liftIO >>> lift

instance MonadBase b m => MonadBase b (ValidateT e m) where
  liftBase = liftBase >>> lift

instance MonadReader r m => MonadReader r (ValidateT e m) where
  ask :: ValidateT e m r
  ask = lift ask

  local :: (r -> r) -> ValidateT e m a -> ValidateT e m a
  local f (ValidateT mr) = local f mr & ValidateT

instance MonadState s m => MonadState s (ValidateT e m) where
  get :: ValidateT e m s
  get = lift get

  put :: s -> ValidateT e m ()
  put s = put s & lift

  state :: (s -> (a, s)) -> ValidateT e m a 
  state f = state f & lift

class Monad m => MonadValidate e m | m -> e where
  errWithCallStack :: HasCallStack => e -> m a
  err :: e -> m a
  correct :: (NonEmptySeq e -> m a) -> m a -> m a
  -- similar to Alternative's (<|>)
  -- orA accumulates errors if both fail
  orA :: m a -> m a -> m a
  -- orM uses the second's errors if both fail
  orM :: m a -> m a -> m a

-- run f a over and over again until f a fails.
-- many :: Alternative f => f a -> f [a]
-- many v = liftA2 (:) v ( ( liftA2 (:) v many_v ) <|> pure [] ) <|> pure []
--   where
--     many_v = some_v <|> pure []
--     some_v = liftA2 (:) v many_v

instance Monad m => MonadValidate e (ValidateT e m) where
  errWithCallStack :: HasCallStack => e -> ValidateT e m a
  errWithCallStack e = Internal.Failures (NonEmptySeq.singleton (Internal.Failure (Just callStack) e)) 
    & pure
    & ValidateT

  err :: e -> ValidateT e m a
  err e = Internal.Failures (NonEmptySeq.singleton (Internal.Failure Nothing e))
    & pure 
    & ValidateT

  correct :: (NonEmptySeq e -> ValidateT e m a) -> ValidateT e m a -> ValidateT e m a
  correct handle (ValidateT mr) = mr 
    >>= \case
      Internal.Failures failures -> failures <&> Internal.failureToError & handle & runValidateT
      success -> success & pure
    & ValidateT

  orA :: ValidateT e m a -> ValidateT e m a -> ValidateT e m a
  orA (ValidateT mr1) (ValidateT mr2) = ValidateT $ or <$> mr1 <*> mr2
    where
      or :: Internal.Result e a -> Internal.Result e a -> Internal.Result e a
      or r1@(Internal.Success _) _ = r1
      or (Internal.Failures failures1) (Internal.Failures failures2) = Internal.Failures (failures1 <> failures2)
      or (Internal.Failures _) r2 = r2

  orM :: ValidateT e m a -> ValidateT e m a -> ValidateT e m a
  orM (ValidateT mr1) (ValidateT mr2) = ValidateT $ or <$> mr1 <*> mr2
    where
      or :: Internal.Result e a -> Internal.Result e a -> Internal.Result e a
      or r1@(Internal.Success _) _ = r1
      or (Internal.Failures _) r2 = r2

toErrors :: (Functor m) => ValidateT e m a -> m (Seq e)
toErrors (ValidateT mr) = mr <&> \case
  Internal.Failures failures -> failures <&> (\(Internal.Failure _ e) -> e) & NonEmptySeq.toSeq
  _ -> Seq.empty

containsError :: (Functor m, Eq e) => e -> ValidateT e m a -> m Bool
containsError error result = toErrors result <&> elem error

mapErrors :: Functor m => (e1 -> e2) -> ValidateT e1 m a -> ValidateT e2 m a
mapErrors f (ValidateT mr) = mr
  <&> \case
    Internal.Failures errors -> Internal.Failures ((fmap . fmap) f errors) 
    Internal.Success a -> Internal.Success a
  & ValidateT

fromJustOrErrWithCallStack :: MonadValidate e m => e -> Maybe a -> m a
fromJustOrErrWithCallStack e Nothing = e & errWithCallStack
fromJustOrErrWithCallStack _ (Just a) = a & pure

fromJustOrErr :: MonadValidate e m => e -> Maybe a -> m a
fromJustOrErr e Nothing = e & err
fromJustOrErr _ (Just a) = a & pure

fromRightOrErrWithCallStack :: MonadValidate e m => (l -> e) -> Either l r -> m r
fromRightOrErrWithCallStack f (Left l) = f l & errWithCallStack
fromRightOrErrWithCallStack _ (Right r) = r & pure

fromRightOrErr :: MonadValidate e m => (l -> e) -> Either l r -> m r
fromRightOrErr f (Left l) = f l & err
fromRightOrErr _ (Right r) = r & pure

fromValidate :: (Applicative m) => Validate e a -> ValidateT e m a
fromValidate (ValidateT (Identity r)) = r & pure & ValidateT

removeCallStacks :: Functor m => ValidateT e m a -> ValidateT e m a
removeCallStacks (ValidateT mr) = mr 
  <&> \case
    Internal.Failures failures ->
      failures
        <&> (\(Internal.Failure _ e) -> Internal.Failure Nothing e)
        & Internal.Failures
    success -> success
  & ValidateT
  
newtype WrappedMonadTrans (t :: (* -> *) -> * -> *) (m :: * -> *) (a :: *)
  = WrappedMonadTrans { unWrappedMonadTrans :: t m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadTransControl)

instance (MonadTransControl t, Monad (t m), MonadValidate e m) => MonadValidate e (WrappedMonadTrans t m) where
  errWithCallStack = lift . errWithCallStack
  err = lift . err

  correct :: (NonEmptySeq e -> WrappedMonadTrans t m a) -> WrappedMonadTrans t m a -> WrappedMonadTrans t m a
  correct handle wmt = liftWith (\run -> correct (run . handle) (run wmt)) >>= restoreT . pure

  orA :: WrappedMonadTrans t m a -> WrappedMonadTrans t m a -> WrappedMonadTrans t m a
  orA wmt1 wmt2 = liftWith (\run -> orA (run wmt1) (run wmt2)) >>= restoreT . pure

  orM :: WrappedMonadTrans t m a -> WrappedMonadTrans t m a -> WrappedMonadTrans t m a
  orM wmt1 wmt2 = liftWith (\run -> orM (run wmt1) (run wmt2)) >>= restoreT . pure

deriving via (WrappedMonadTrans IdentityT m) instance MonadValidate e m => MonadValidate e (IdentityT m)
deriving via (WrappedMonadTrans (ReaderT r) m) instance MonadValidate e m => MonadValidate e (ReaderT r m)
deriving via (WrappedMonadTrans (Lazy.StateT s) m) instance MonadValidate e m => MonadValidate e (Lazy.StateT s m)
deriving via (WrappedMonadTrans (Strict.StateT s) m) instance MonadValidate e m => MonadValidate e (Strict.StateT s m)
