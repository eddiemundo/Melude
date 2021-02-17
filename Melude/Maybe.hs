module Melude.Maybe 
  ( toEither
  , toBool
  , validate
  , justOrElse
  , justOrThrow
  , module BaseMaybe
  ) where

import Data.Maybe as BaseMaybe 
import Control.Exception (throw, Exception)

toEither :: l -> Maybe r -> Either l r
toEither l = \case
  Nothing -> Left l
  Just r -> Right r

toBool :: Maybe a -> Bool
toBool = \case
  Nothing -> False
  Just _ -> True

justOrElse :: a -> Maybe a -> a
justOrElse = fromMaybe

justOrThrow :: Exception e => e -> Maybe a -> a
justOrThrow e = justOrElse (throw e)

validate :: (a -> Bool) -> a -> Maybe a
validate predicate a
  | predicate a = Just a
  | otherwise = Nothing
