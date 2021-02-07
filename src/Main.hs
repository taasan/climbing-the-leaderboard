{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Data.Foldable
  ( traverse_
  )
import Prelude
  ( error
  )
import PsPrelude

import System.Environment
  ( getArgs
  )
import Lib
import qualified Lib.Foldl as Foldl
import qualified Lib.TailCall as TailCall


main :: Effect Unit
main = do
  args <- getArgs
  case args of
    ["--list"] -> do
      traverse_ log ["tc", "foldl"]
    _ -> do
      let
        readScores = getLine >>= pure <<< map read <<< words
      _ <- getLine
      ranked <- readScores
      _ <- getLine
      player <- readScores
      let
        solver
          = case args of
            ["--tc"] -> TailCall.solve
            ["--foldl"] -> Foldl.solve
            _ -> error "No soup for you!"
      doSolve solver (RankedScores ranked) (PlayerScores player)

doSolve :: (a -> b -> Climb Rank) -> a -> b -> Effect Unit
doSolve solver highscore player = traverse_ logShow (solver highscore player)
