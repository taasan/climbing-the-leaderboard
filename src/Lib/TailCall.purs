{-
#if 0
-}
module Lib.TailCall where

import Prelude
import Data.List (List(..), reverse, snoc, zip, (:))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Lib (Climb(..), PlayerScores(..), Rank, RankedScores(..), Score, ScoreBoard(..), ScoreBoardList, succ, tail)

{-
#endif
-}
positions :: RankedScores -> ScoreBoard
positions (RankedScores ranks) = ScoreBoard $ g one (zip ranks $ snoc (tail ranks) zero) mempty
  where
  g :: Rank -> List (Tuple Score Score) -> ScoreBoardList -> ScoreBoardList
  g _ Nil acc = acc

  g rank ((Tuple current next) : xs) acc
    | current == next = g rank xs acc
    | otherwise = g (succ rank) xs $ Tuple rank current : acc

climb :: RankedScores -> PlayerScores -> List Rank
climb (RankedScores Nil) _ = Nil

climb rankedScores@(RankedScores (highScore : _)) (PlayerScores player) = go player ranked mempty
  where
  ranked = unwrap <<< positions $ rankedScores

  go :: List Score -> ScoreBoardList -> List Rank -> List Rank
  go Nil _ acc = acc

  go _ Nil acc = one : acc

  go ps@(score : ps') rs@((Tuple rank score') : rs') acc
    | score >= highScore = go ps' rs $ one : acc -- 🏆
    | score < score' = go ps' rs $ succ rank : acc
    | otherwise = go ps rs' acc
      where
      p (Tuple _ x) = score >= x

solve :: RankedScores -> PlayerScores -> Climb Rank
solve = (compose (Climb <<< reverse)) <<< climb
