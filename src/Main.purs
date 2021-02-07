module Main where

import Data.Filterable (filterMap)
import Data.Foldable (indexl)
import Data.Int (fromString)
import Data.List (List(..), drop, fromFoldable, (:))
import Data.Maybe (fromJust)
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Lib (PlayerScores(..), RankedScores(..), Score(..), doSolve, lines, printLines, words)
import Lib.Foldl as Foldl
import Lib.MonadRec as MonadRec
import Lib.TailCall as TailCall
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Node.Process (argv, exit)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, bind, discard, identity, map, pure, ($), (<$>), (<<<), (>>=))

main :: Effect Unit
main = do
  let
    printLines' f = printLines identity <<< unwrap <<< f
  solver <-
    argv
      >>= \xs ->
          pure case (drop 2 <<< fromFoldable) xs of
            Nil -> doSolve Foldl.solve
            ("--mr" : Nil) -> doSolve MonadRec.solve
            ("--mr-pos" : Nil) -> \ranked _ -> printLines' MonadRec.positions ranked
            ("--tc" : Nil) -> doSolve TailCall.solve
            ("--tc-pos" : Nil) -> \ranked _ -> printLines' TailCall.positions ranked
            ("--foldl" : Nil) -> doSolve Foldl.solve
            ("--foldl-pos" : Nil) -> \ranked _ -> printLines' Foldl.positions ranked
            _ -> \_ _ -> do
              logShow "No soup for you!"
              exit 1
  xs :: List (List Int) <- map (filterMap fromString <<< words) <$> lines <$> readTextFile UTF8 "/dev/stdin"
  let
    f x = unsafePartial $ fromJust $ indexl x xs

    ranked = RankedScores <<< map Score <<< f $ 1

    player = PlayerScores <<< map Score <<< f $ 3
  solver ranked player
