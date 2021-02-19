{-# LANGUAGE FunctionalDependencies #-}
module Melude.ValidateT.Internal where

import Prelude hiding (fail, error)
import GHC.Stack (CallStack, prettyCallStack)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Sequence.NonEmpty (NESeq)
import qualified Data.Sequence.NonEmpty as NonEmptySeq
import Data.Functor ((<&>))
import Data.Function ((&))
import Data.Foldable (Foldable(fold))

type NonEmptySeq a = NESeq a

data Failure e = Failure !(Maybe CallStack) !e
  deriving Functor

data Result e a
  = Failures !(NonEmptySeq (Failure e))
  | Success a
  deriving Functor

-- pattern Empty :: Result e a
-- pattern Empty = Errors Seq.Empty

-- pattern Cons :: e -> Seq e -> Result e a
-- pattern Cons error remainingErrors <- (toErrors -> error Seq.:<| remainingErrors)

-- pattern Snoc :: Seq e -> e -> Result e a
-- pattern Snoc remainingErrors error <- (toErrors -> remainingErrors Seq.:|> error)

-- toErrors :: Result e a -> Seq e
-- toErrors (Errors errors) = fmap (\(Error _ e) -> e) errors
-- toErrors _ = Seq.empty

-- containsError :: Eq e => e -> Result e a -> Bool
-- containsError error result
--   | elem error errors = True
--   | otherwise = False
--   where
--     errors = toErrors result

-- {-# COMPLETE Empty, Cons #-}
-- {-# COMPLETE Empty, Snoc #-}
failureToError :: Failure e -> e
failureToError (Failure _ e) = e

failureToText :: Show e => Failure e -> Text
failureToText (Failure (Just stack) e) = Text.pack (show e) <> "\n" <> Text.pack (prettyCallStack stack)
failureToText (Failure Nothing e) = Text.pack (show e)

failuresToText :: Show e => NonEmptySeq (Failure e) -> Text
failuresToText failures = failures <&> failureToText & NonEmptySeq.intersperse "\n" & fold

instance (Show e, Show a) => Show (Result e a) where
  show (Failures failures) = "Failures:\n" <> failuresToText failures & Text.unpack
  show (Success a) = "Success(" <> show a <> ")"

instance Applicative (Result e) where
  pure = Success

  (<*>) (Failures leftErrors) (Failures rightErrors) = Failures (leftErrors <> rightErrors)
  (<*>) (Failures failures) _ = Failures failures
  (<*>) _ (Failures failures) = Failures failures
  (<*>) (Success f) (Success a) = Success (f a)

instance Monad (Result e) where
  (>>=) (Failures failures) _ = Failures failures
  (>>=) (Success a) f = f a

