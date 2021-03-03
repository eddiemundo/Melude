{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
module Melude.ValidateT where

import Prelude hiding (error, or)
import Control.Monad.State.Lazy as Lazy
import Control.Monad.State.Strict as Strict
import GHC.Stack (HasCallStack, callStack, CallStack)
import Control.Applicative (Applicative(liftA2))
import Data.Function ((&))
import Data.Functor ((<&>))
import Control.Monad.Reader (MonadReader(ask, local), ReaderT)
import Data.Sequence (Seq)
import Control.Category ((>>>))
import Control.Monad.Trans.Control (MonadTransControl (liftWith, restoreT), MonadBaseControl)
import Control.Monad.Identity (IdentityT)
import Control.Monad.Base (MonadBase(liftBase))
import qualified Data.Sequence as Seq
import Data.Functor.Identity (Identity(Identity))
import qualified Data.Sequence.NonEmpty as NonEmptySeq
import Melude.Result (Result(Success, Failures), Failure(Failure), NonEmptySeq)
import Melude.Result (NonEmptySeq)
import qualified Melude.Result as Result

-- monad-validate
import qualified Control.Monad.Validate as Internal
import qualified Control.Monad.Validate.Internal as Internal

import Control.Monad.Writer.Class (MonadWriter)

-- Notes:
-- If we want MonadBaseControl then write a MonadTransControl instance.
newtype ValidateT e m a = ValidateT (Internal.ValidateT (NonEmptySeq (Failure e)) m a)
  deriving 
  ( Functor
  , Applicative
  , Monad
  , MonadIO
  , MonadTrans
  , MonadTransControl
  , MonadBase b
  , MonadBaseControl b
  , MonadWriter w
  , MonadState s
  , MonadReader r
  , Internal.MonadValidate (NonEmptySeq (Failure e))
  )

class Monad m => MonadValidateExtension e m | m -> e where
  tolerate :: m a -> m (Either (Failures e) a)

instance Monad m => MonadValidateExtension e (ValidateT e m) where
  tolerate :: ValidateT e m a -> ValidateT e m (Either (Failures e) a)
  tolerate (ValidateT m) = ValidateT $ Internal.validateT $ \e1 ->
    Right . either (\e2 -> (Internal.MJust e2, Left e2)) (fmap Right) <$> Internal.unValidateT e1 m

runValidateT :: Functor m => ValidateT e m a -> m (Either (Failures e) a)
runValidateT (ValidateT internal) = Internal.runValidateT internal

type Validate e a = ValidateT e Identity a

type Failures e = NonEmptySeq (Failure e)

type MonadValidate e m = (HasCallStack, Internal.MonadValidate (Failures e) m, MonadValidateExtension e m)

err :: MonadValidate e m => e -> m a
err e = Internal.refute $ NonEmptySeq.singleton (Failure callStack e)

errs :: MonadValidate e m => NonEmptySeq e -> m a
errs es = Internal.refute $ es <&> Failure callStack

tolerateAsMaybe :: MonadValidate e m => m a -> m (Maybe a)
tolerateAsMaybe = Internal.tolerate


fromJustOrErr :: MonadValidate e m => e -> Maybe a -> m a
fromJustOrErr e Nothing = e & err
fromJustOrErr _ (Just a) = a & pure

fromRightOrErr :: MonadValidate e m => (l -> e) -> Either l r -> m r
fromRightOrErr f (Left l) = f l & err
fromRightOrErr _ (Right r) = r & pure


-- fromResult :: Applicative m => Result e a -> ValidateT e m a
-- fromResult result = result & pure & ValidateT

-- fromValidate :: (Applicative m) => Validate e a -> ValidateT e m a
-- fromValidate (ValidateT (Identity r)) = fromResult r


-- instance Applicative m => Applicative (ValidateT e m) where
--   pure :: a -> ValidateT e m a
--   pure a = a & pure & pure & ValidateT

--   (<*>) :: ValidateT e m (a -> b) -> ValidateT e m a -> ValidateT e m b
--   (<*>) (ValidateT mf) (ValidateT ma) = ValidateT $ liftA2 (<*>) mf ma
     
-- instance Monad m => Monad (ValidateT e m) where
--   (>>=) :: ValidateT e m a -> (a -> ValidateT e m b) -> ValidateT e m b
--   (>>=) (ValidateT mr) f = ValidateT $ mr >>= \case
--     Success a -> f a & runValidateT
--     Failures failures -> Failures failures & pure

-- instance MonadTrans (ValidateT e) where
--   lift :: Functor m => m a -> ValidateT e m a
--   lift ma = ma <&> pure & ValidateT

-- instance MonadIO m => MonadIO (ValidateT e m) where
--   liftIO :: IO a -> ValidateT e m a
--   liftIO = liftIO >>> lift

-- instance MonadBase b m => MonadBase b (ValidateT e m) where
--   liftBase :: b a -> ValidateT e m a
--   liftBase = liftBase >>> lift

-- instance MonadReader r m => MonadReader r (ValidateT e m) where
--   ask :: ValidateT e m r
--   ask = lift ask

--   local :: (r -> r) -> ValidateT e m a -> ValidateT e m a
--   local f (ValidateT mr) = local f mr & ValidateT

-- instance MonadState s m => MonadState s (ValidateT e m) where
--   get :: ValidateT e m s
--   get = lift get

--   put :: s -> ValidateT e m ()
--   put s = put s & lift

--   state :: (s -> (a, s)) -> ValidateT e m a 
--   state f = state f & lift

-- class Monad m => MonadValidate e m | m -> e where
--   err :: HasCallStack => e -> m a
--   correct :: (NonEmptySeq e -> m a) -> m a -> m a
--   -- similar to Alternative's (<|>)
--   -- orA accumulates errors if both fail
--   orA :: m a -> m a -> m a
--   -- orM uses the second's errors if both fail
--   -- orM :: m a -> m a -> m a

-- -- --  reifyErrors :: m a -> m (Result e a)

-- instance Monad m => MonadValidate e (ValidateT e m) where
--   err :: HasCallStack => e -> ValidateT e m a
--   err e = Failures (NonEmptySeq.singleton (Failure callStack e))
--     & pure 
--     & ValidateT

--   errs :: HasCallStack => NonEmptySeq e -> ValidateT e m a
--   errs es = Failures 

--   correct :: (NonEmptySeq (Failure e) -> ValidateT e m a) -> ValidateT e m a -> ValidateT e m a
--   correct handle (ValidateT mr) = ValidateT $ mr >>= \case
--     Failures failures -> failures & handle & runValidateT
--     success -> success & pure

--   orA :: ValidateT e m a -> ValidateT e m a -> ValidateT e m a
--   orA (ValidateT mr1) (ValidateT mr2) = ValidateT $ or <$> mr1 <*> mr2
--     where
--       or :: Result e a -> Result e a -> Result e a
--       or r1@(Success _) _ = r1
--       or (Failures failures1) (Failures failures2) = Failures (failures1 <> failures2)
--       or (Failures _) r2 = r2

--   orM :: ValidateT e m a -> ValidateT e m a -> ValidateT e m a
--   orM (ValidateT mr1) (ValidateT mr2) = ValidateT $ or <$> mr1 <*> mr2
--     where
--       or :: Result e a -> Result e a -> Result e a
--       or r1@(Success _) _ = r1
--       or (Failures _) r2 = r2

-- orA :: MonadValidate e m => m a -> m a -> m a
-- orA l r = l & correct (\failures -> Failures failures <>   )

-- tolerate :: MonadValidate e m => m a -> m (Result e a)
-- tolerate ma = correct (\failures -> Failures failures & pure) (ma <&> Success)

-- toErrors :: (Functor m) => ValidateT e m a -> m (Seq e)
-- toErrors (ValidateT mr) = mr <&> \case
--   Failures failures -> failures <&> (\(Failure _ e) -> e) & NonEmptySeq.toSeq
--   _ -> Seq.empty

-- containsError :: (Functor m, Eq e) => e -> ValidateT e m a -> m Bool
-- containsError error result = toErrors result <&> elem error

-- mapErrors :: Functor m => (e1 -> e2) -> ValidateT e1 m a -> ValidateT e2 m a
-- mapErrors f (ValidateT mr) = mr
--   <&> \case
--     Failures errors -> Failures ((fmap . fmap) f errors) 
--     Success a -> Success a
--   & ValidateT

-- fromJustOrErrWithCallStack :: MonadValidate e m => e -> Maybe a -> m a
-- fromJustOrErrWithCallStack e Nothing = e & errWithCallStack
-- fromJustOrErrWithCallStack _ (Just a) = a & pure

-- fromJustOrErr :: MonadValidate e m => e -> Maybe a -> m a
-- fromJustOrErr e Nothing = e & err
-- fromJustOrErr _ (Just a) = a & pure

-- fromRightOrErrWithCallStack :: MonadValidate e m => (l -> e) -> Either l r -> m r
-- fromRightOrErrWithCallStack f (Left l) = f l & errWithCallStack
-- fromRightOrErrWithCallStack _ (Right r) = r & pure

-- fromRightOrErr :: MonadValidate e m => (l -> e) -> Either l r -> m r
-- fromRightOrErr f (Left l) = f l & err
-- fromRightOrErr _ (Right r) = r & pure

-- fromResult :: Applicative m => Result e a -> ValidateT e m a
-- fromResult result = result & pure & ValidateT

-- fromValidate :: (Applicative m) => Validate e a -> ValidateT e m a
-- fromValidate (ValidateT (Identity r)) = fromResult r

-- removeCallStacks :: Functor m => ValidateT e m a -> ValidateT e m a
-- removeCallStacks (ValidateT mr) = mr 
--   <&> \case
--     Failures failures ->
--       failures
--         <&> (\(Failure _ e) -> Failure Nothing e)
--         & Failures
--     success -> success
--   & ValidateT
  
newtype WrappedMonadTrans (t :: (* -> *) -> * -> *) (m :: * -> *) (a :: *)
  = WrappedMonadTrans { unWrappedMonadTrans :: t m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadTransControl)

instance (MonadTransControl t, Monad (t m), MonadValidateExtension e m) => MonadValidateExtension e (WrappedMonadTrans t m) where
  tolerate :: WrappedMonadTrans t m a -> WrappedMonadTrans t m (Either (Failures e) a)
  tolerate wmt1 = liftWith (\run -> tolerate (run wmt1)) >>= either (pure . Left) (fmap Right . restoreT . pure)
  -- errWithCallStack = lift . errWithCallStack
  -- err = lift . err

  -- correct :: (NonEmptySeq (Failure e) -> WrappedMonadTrans t m a) -> WrappedMonadTrans t m a -> WrappedMonadTrans t m a
  -- correct handle wmt = liftWith (\run -> correct (run . handle) (run wmt)) >>= restoreT . pure

  -- orA :: WrappedMonadTrans t m a -> WrappedMonadTrans t m a -> WrappedMonadTrans t m a
  -- orA wmt1 wmt2 = liftWith (\run -> orA (run wmt1) (run wmt2)) >>= restoreT . pure

  -- orM :: WrappedMonadTrans t m a -> WrappedMonadTrans t m a -> WrappedMonadTrans t m a
  -- orM wmt1 wmt2 = liftWith (\run -> orM (run wmt1) (run wmt2)) >>= restoreT . pure

deriving via (WrappedMonadTrans IdentityT m) instance MonadValidateExtension e m => MonadValidateExtension e (IdentityT m)
deriving via (WrappedMonadTrans (ReaderT r) m) instance MonadValidateExtension e m => MonadValidateExtension e (ReaderT r m)
deriving via (WrappedMonadTrans (Lazy.StateT s) m) instance MonadValidateExtension e m => MonadValidateExtension e (Lazy.StateT s m)
deriving via (WrappedMonadTrans (Strict.StateT s) m) instance MonadValidateExtension e m => MonadValidateExtension e (Strict.StateT s m)
