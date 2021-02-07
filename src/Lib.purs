{-
#if 0
-}
module Lib where

import Prelude
import Data.Enum (class Enum, pred)
import Data.Enum as Enum
import Data.Foldable (class Foldable, intercalate, foldl, foldMap, foldr)
import Data.List (List, fromFoldable)
import Data.List as List
import Data.Maybe (fromJust)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String (Pattern(..))
import Data.String.Regex (split)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)

{-
#endif
-}
-- Create some newtypes to make sure we pass arguments in the correct order
newtype PlayerScores
  = PlayerScores (List Score)

newtype Score
  = Score Int

newtype RankedScores
  = RankedScores (List Score)

newtype Rank
  = Rank Int

type ScoreBoardList
  = List (Tuple Rank Score)

newtype ScoreBoard
  = ScoreBoard ScoreBoardList

newtype Climb a
  = Climb (List a)

{-
#if 0
-}
doSolve :: forall a b. (a -> b -> Climb Rank) -> a -> b -> Effect Unit
doSolve solver highscore player = printLines identity $ solver highscore player

printLines :: forall f a b. Foldable f => Functor f => Show b => (a -> f b) -> a -> Effect Unit
printLines f = log <<< intercalate "\n" <<< map show <<< f

{- Instances -}
derive instance newtypePlayer :: Newtype PlayerScores _

derive instance newtypeScore :: Newtype Score _

derive instance newtypeScores :: Newtype RankedScores _

derive instance newtypeRank :: Newtype Rank _

derive instance newtypeScoreBoard :: Newtype ScoreBoard _

derive instance newtypeClimb :: Newtype (Climb a) _

derive instance functorClimb :: Functor Climb

derive instance ordScore :: Ord Score

derive instance ordRank :: Ord Rank

derive instance eqScore :: Eq Score

derive instance eqRank :: Eq Rank

instance enumScore :: Enum Score where
  succ = newtypeHelper1 Enum.succ
  pred = newtypeHelper1 pred

instance enumRank :: Enum Rank where
  succ = newtypeHelper1 Enum.succ
  pred = newtypeHelper1 pred

instance showRank :: Show Rank where
  show = show <<< unwrap

instance showScore :: Show Score where
  show = show <<< unwrap

instance semiringRank :: Semiring Rank where
  zero = Rank zero
  one = Rank one
  add (Rank x) (Rank y) = Rank $ add x y
  mul (Rank x) (Rank y) = Rank $ mul x y

instance semiringScore :: Semiring Score where
  zero = Score zero
  one = Score one
  add (Score x) (Score y) = Score $ add x y
  mul (Score x) (Score y) = Score $ mul x y

instance foldableClimb :: Foldable Climb where
  foldl f i (Climb xs) = foldl f i xs
  foldr f i (Climb xs) = foldr f i xs
  foldMap f (Climb xs) = foldMap f xs

{- Haskell compat -}
tail :: forall a. List a -> List a
tail xs = unsafePartial $ fromJust $ List.tail xs

-- Crash on overflow
succ :: forall a. Enum a => a -> a
succ x = unsafePartial $ fromJust $ Enum.succ x

lines :: String -> List String
lines str = fromFoldable $ unsafeSplit (Pattern "\n") str

words :: String -> List String
words str = fromFoldable $ unsafeSplit (Pattern " ") str

unsafeSplit :: Pattern -> String -> Array String
unsafeSplit (Pattern pattern) = split re
  where
  re = unsafeRegex pattern noFlags

-- | Simplifies working with newtypes
newtypeHelper1 ::
  forall f t a b s.
  Functor f =>
  Newtype t a =>
  Newtype s b =>
  (b -> f a) ->
  s ->
  f t
newtypeHelper1 f = map wrap <$> f <<< unwrap

{-
#endif
-}
