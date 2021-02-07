{-
#if 0
-}
module Lib.Foldl where

import Prelude
import Data.List (List(..), foldl, reverse, (:))
import Data.Newtype (unwrap, wrap)
import Data.Tuple (Tuple(..))
import Lib (Climb(..), PlayerScores(..), Rank, RankedScores(..), Score, ScoreBoard, ScoreBoardList, succ)

{-
#endif
-}
positions :: RankedScores -> ScoreBoard
positions (RankedScores ranks) = wrap $ foldl f mempty ranks
  where
  f :: ScoreBoardList -> Score -> ScoreBoardList
  f Nil score = pure $ Tuple one score

  f acc@(Tuple rank current : _) score
    | current == score = acc
    | otherwise = Tuple (succ rank) score : acc

climb :: RankedScores -> PlayerScores -> List Rank
climb (RankedScores Nil) _ = Nil

climb xs@(RankedScores (highScore : _)) (PlayerScores ys) = go ys ranked mempty
  where
  ranked = unwrap $ positions xs

  go :: List Score -> ScoreBoardList -> List Rank -> List Rank
  go Nil _ acc = acc

  go _ Nil acc = one : acc

  go ps@(score : ps') rs@((Tuple rank score') : rs') acc
    | score >= highScore = go ps' rs $ one : acc -- #1!
    | score < score' = go ps' rs $ succ rank : acc
    | otherwise = go ps rs' acc

solve :: RankedScores -> PlayerScores -> Climb Rank
-- Good luck trying to understand this the next time you see it ;)
solve = (compose (Climb <<< reverse)) <<< climb
