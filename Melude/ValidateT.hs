{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
module Melude.ValidateT where

import Control.Monad.State.Lazy as Lazy
import Control.Monad.State.Strict as Strict
import GHC.Stack (HasCallStack, callStack, CallStack, prettyCallStack)
import Data.Function ((&))
import Data.Functor ((<&>))
import Control.Monad.Writer.Class (MonadWriter)
import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Monad.Trans.Control (MonadTransControl (liftWith, restoreT), MonadBaseControl)
import Control.Monad.Identity (IdentityT)
import Control.Monad.Base (MonadBase)
import Data.Functor.Identity (Identity)
import qualified Data.Sequence.NonEmpty as NonEmptySeq

-- monad-validate
import qualified Control.Monad.Validate as Internal
import qualified Control.Monad.Validate.Internal as Internal

-- prettyprinter
import Prettyprinter (Pretty(pretty), vsep)

import Melude.NonEmptySeq (NonEmptySeq)

data Failure e = Failure !CallStack !e
  deriving Functor

instance Pretty e => Pretty (Failure e) where
  pretty (Failure stack e) = [pretty e, pretty $ prettyCallStack stack] & vsep

type Failures e = NonEmptySeq (Failure e)

newtype ValidateT e m a = ValidateT (Internal.ValidateT (Failures e) m a)
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
  , Internal.MonadValidate (Failures e)
  )

type Validate e a = ValidateT e Identity a

runValidateT :: Functor m => ValidateT e m a -> m (Either (Failures e) a)
runValidateT (ValidateT internal) = Internal.runValidateT internal

class Monad m => MonadValidateExtension e m | m -> e where
  tolerate :: m a -> m (Either (Failures e) a)

instance Monad m => MonadValidateExtension e (ValidateT e m) where
  tolerate :: ValidateT e m a -> ValidateT e m (Either (Failures e) a)
  tolerate (ValidateT m) = lift $ Internal.runValidateT m  

type MonadValidate e m = (HasCallStack, Internal.MonadValidate (Failures e) m, MonadValidateExtension e m)

correct :: MonadValidate e m => (NonEmptySeq (Failure e) -> m a) -> m a -> m a
correct f ma = tolerate ma >>= \case
  Left failures -> f failures
  Right _ -> ma

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

orM :: MonadValidate e m => m a -> m a -> m a
orM ma1 ma2 = correct (const ma2) ma1

orA :: MonadValidate e m => m a -> m a -> m a
orA ma1 ma2 = correct (\failures1 -> correct (\failures2 -> Internal.refute (failures1 <> failures2)) ma2) ma1

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

newtype WrappedMonadTrans (t :: (* -> *) -> * -> *) (m :: * -> *) (a :: *)
  = WrappedMonadTrans { unWrappedMonadTrans :: t m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadTransControl)

instance (MonadTransControl t, Monad (t m), MonadValidateExtension e m) => MonadValidateExtension e (WrappedMonadTrans t m) where
  tolerate :: WrappedMonadTrans t m a -> WrappedMonadTrans t m (Either (Failures e) a)
  tolerate wmt1 = liftWith (\run -> tolerate (run wmt1)) >>= either (pure . Left) (fmap Right . restoreT . pure)

deriving via (WrappedMonadTrans IdentityT m) instance MonadValidateExtension e m => MonadValidateExtension e (IdentityT m)
deriving via (WrappedMonadTrans (ReaderT r) m) instance MonadValidateExtension e m => MonadValidateExtension e (ReaderT r m)
deriving via (WrappedMonadTrans (Lazy.StateT s) m) instance MonadValidateExtension e m => MonadValidateExtension e (Lazy.StateT s m)
deriving via (WrappedMonadTrans (Strict.StateT s) m) instance MonadValidateExtension e m => MonadValidateExtension e (Strict.StateT s m)
