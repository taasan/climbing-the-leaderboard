module Lib.MonadRec where

import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.List (List(..), head, reverse, snoc, zip, (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.Tuple (Tuple(..), snd)
import Lib (Climb, PlayerScores(..), Rank, RankedScores(..), Score, ScoreBoard, ScoreBoardList, succ, tail)
import Prelude (compose, mempty, one, otherwise, zero, ($), (<), (<<<), (==), (>=))

positions :: RankedScores -> ScoreBoard
positions (RankedScores player) =
  wrap
    $ tailRec g
        { acc: Nil
        , rank: one
        , ranked: zip player $ snoc (tail player) zero
        }
  where
  g :: _ -> Step _ ScoreBoardList
  g { acc, ranked: Nil } = Done acc

  g { acc, rank, ranked: ((Tuple current next) : ranked) } =
    Loop
      { rank: if current == next then rank else succ rank
      , ranked
      , acc: acc'
      }
    where
    acc' = if head acc == Just nt then acc else nt : acc

    nt = Tuple rank current

climb :: RankedScores -> PlayerScores -> List Rank
climb (RankedScores Nil) _ = Nil

climb ranked@(RankedScores (highScore : _)) (PlayerScores ys) =
  tailRec go
    { player: ys
    , ranked: unwrap $ positions ranked
    , acc: mempty
    }
  where
  go :: { player :: List Score, ranked :: ScoreBoardList, acc :: List Rank } -> Step _ _
  go { player: Nil, ranked: _, acc } = Done acc

  go { player: _, ranked: Nil, acc } = Done acc

  go { player: ps@(score : ps'), ranked: rs@((Tuple rank score') : rs'), acc }
    | score >= highScore =
      Loop
        { player: ps'
        , ranked: rs
        , acc: one : acc
        }
    | score < score' =
      Loop
        { player: ps'
        , ranked: rs
        , acc: succ rank : acc
        }
    | otherwise =
      Loop
        { player: ps
        , ranked: rs'
        , acc
        }
      where
      p = (score >= _) <<< snd

solve :: RankedScores -> PlayerScores -> Climb Rank
solve = (compose (wrap <<< reverse)) <<< climb
