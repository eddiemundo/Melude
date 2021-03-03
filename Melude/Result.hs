{-# LANGUAGE FunctionalDependencies #-}
module Melude.Result where

import Prelude hiding (fail, error)
import GHC.Stack (CallStack, prettyCallStack, HasCallStack, callStack)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Sequence.NonEmpty (NESeq)
import qualified Data.Sequence.NonEmpty as NonEmptySeq
import Data.Functor ((<&>))
import Data.Function ((&))
import Data.Foldable (Foldable(fold))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Bifoldable (Bifoldable(bifoldr))
import Prettyprinter (Pretty(pretty), vsep)

type NonEmptySeq a = NESeq a

data Failure e = Failure !CallStack !e
  deriving Functor

instance Pretty e => Pretty (Failure e) where
  pretty (Failure stack e) = vsep [pretty e, pretty $ prettyCallStack stack]

data Result e a
  = Failures !(NonEmptySeq (Failure e))
  | Success a
  deriving Functor

-- instance Show e => Show (Failure e) where
--   show failure = failureToText failure & Text.unpack

-- instance (Show e, Show a) => Show (Result e a) where
--   show (Failures failures) = "Failures:\n" <> failuresToText failures & Text.unpack
--   show (Success a) = "Success(" <> show a <> ")"

instance Applicative (Result e) where
  pure :: a -> Result e a
  pure = Success

  (<*>) :: Result e (a -> b) -> Result e a -> Result e b
  (<*>) (Failures failures1) (Failures failures2) = Failures (failures1 <> failures2)
  (<*>) (Failures failures) _ = Failures failures
  (<*>) _ (Failures failures) = Failures failures
  (<*>) (Success f) (Success a) = Success (f a)

-- instance Monad (Result e) where
--   (>>=) (Failures failures) _ = Failures failures
--   (>>=) (Success a) f = f a

-- instance Bifoldable Result where
--   bifoldr _ g z (Success a) = g a z
--   bifoldr f _ z (Failures failures) = failures <&> failureToError & foldr f z

-- pattern Fail :: e -> CallStack -> Result e a
-- pattern Fail e cs = Failures ((Failure (Just cs) e) :<|| Seq.Empty)

-- fromErrorWithCallStack :: HasCallStack => e -> Result e a
-- fromErrorWithCallStack error = error & Failure (Just callStack) & NonEmptySeq.singleton & Failures

-- fromError :: e -> Result e a
-- fromError error = error & Failure Nothing & NonEmptySeq.singleton & Failures

-- fromJustOrErrWithCallStack :: HasCallStack => e -> Maybe a -> Result e a
-- fromJustOrErrWithCallStack _ (Just a) = Success a
-- fromJustOrErrWithCallStack e Nothing = fromErrorWithCallStack e

-- fromRightOrErrWithCallStack :: HasCallStack => (l -> e) -> Either l r -> Result e r
-- fromRightOrErrWithCallStack _ (Right a) = Success a
-- fromRightOrErrWithCallStack handle (Left l) = handle l & fromErrorWithCallStack

-- toMaybe :: Result e a -> Maybe a
-- toMaybe (Success a) = Just a
-- toMaybe (Failures _) = Nothing

-- toEither :: Result e a -> Either (NonEmptySeq (Failure e)) a
-- toEither (Success a) = Right a 
-- toEither (Failures failures) = Left failures

-- toErrors :: Result e a -> Seq e
-- toErrors (Failures failures) = failures <&> failureToError & NonEmptySeq.toSeq
-- toErrors _ = Seq.empty

failureToError :: Failure e -> e
failureToError (Failure _ e) = e

failureToText :: Show e => Failure e -> Text
failureToText (Failure stack e) = Text.pack (show e) <> "\n" <> Text.pack (prettyCallStack stack)

failuresToText :: Show e => NonEmptySeq (Failure e) -> Text
failuresToText failures = failures <&> failureToText & NonEmptySeq.intersperse "\n" & fold

-- failuresToText :: Show e => NonEmptySeq (Failure e) -> Text
-- failuresToText failures = failures <&> failureToText & NonEmptySeq.intersperse "\n" & fold

