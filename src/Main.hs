-- Purescript compat
-- https://github.com/purescript/documentation/blob/master/language/Differences-from-Haskell.md
--{-# LANGUAGE ApplicativeDo #-}
--{-# LANGUAGE BlockArguments #-}
--{-# LANGUAGE DataKinds #-}
--{-# LANGUAGE DeriveFunctor #-}
--{-# LANGUAGE DeriveGeneric #-}
--{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExplicitForAll #-}
--{-# LANGUAGE FlexibleContexts #-}
--{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
--{-# LANGUAGE InstanceSigs #-}
--{-# LANGUAGE KindSignatures #-}
--{-# LANGUAGE LambdaCase #-}
--{-# LANGUAGE LiberalTypeSynonyms #-}
--{-# LANGUAGE MonoLocalBinds #-}
--{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
--{-# LANGUAGE NumericUnderscores #-}
--{-# LANGUAGE PartialTypeSignatures #-}
--{-# LANGUAGE PolyKinds #-}
--{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE RebindableSyntax #-}
--{-# LANGUAGE RoleAnnotations #-}
--{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
--{-# LANGUAGE StandaloneKindSignatures #-}
--{-# LANGUAGE TypeOperators #-}
--{-# LANGUAGE TypeSynonymInstances #-}
--{-# LANGUAGE UndecidableInstances #-}
--{-# LANGUAGE UndecidableSuperClasses #-}
--{-# LANGUAGE UnicodeSyntax #-}

-- So we can pattern match on Tuple and Nil
{-# LANGUAGE PatternSynonyms #-}

-- For Read and Show instances
{-# LANGUAGE DerivingStrategies #-}

{-# LANGUAGE CPP #-}

module Main where

import Data.Foldable
  ( foldl'
  , traverse_
  )
import Prelude
  ( Enum
  , Eq
  , Foldable
  , Functor
  , IO
  , Int
  , Num
  , Ord
  , Read
  , Show
  , String
  , fmap
  , getLine
  , id
  , mempty
  , otherwise
  , print
  , pure
  , putStrLn
  , read
  , reverse
  , succ
  , tail
  , words
  , zip
  , ($)
  , (.)
  , (<)
  , (==)
  , (>=)
  , (>>=)
  , (<>)
  )

import Data.Coerce
  ( Coercible,
    coerce,
  )

#include "Lib.purs"

#ifdef Foldl
#include "Lib/Foldl.purs"
#else
#include "Lib/TailCall.purs"
#endif
-- PS compat. This makes included Purescript files compile.

-- Read and Show as wrapped type.
-- Instead of `(Score 12)`, we read and show `12`
deriving newtype instance Show Score

deriving newtype instance Read Score

deriving instance Num Score

deriving instance Eq Score

deriving instance Ord Score

deriving instance Num Rank

deriving instance Enum Rank

deriving newtype instance Show Rank

deriving instance Foldable Climb

deriving instance Functor Climb

deriving instance Show a => Show (Climb a)

deriving instance Show PlayerScores

deriving instance Show RankedScores

deriving instance Show ScoreBoard

(<<<) :: (b -> c) -> (a -> b) -> a -> c
(<<<) = (.)

compose :: (b -> c) -> (a -> b) -> a -> c
compose = (.)

zero :: Num a => a
zero = 0

one :: Num a => a
one = 1

snoc :: [a] -> a -> [a]
snoc xs x = xs <> [x]

foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
foldl = foldl'

map :: Functor f => (a -> b) -> f a -> f b
map = fmap

logShow :: Show a => a -> IO ()
logShow = print

log :: String -> IO ()
log = putStrLn

wrap :: Coercible a b => a -> b
wrap = coerce

unwrap :: Coercible a b => b -> a
unwrap = coerce

identity :: a -> a
identity = id

type List = []

pattern Nil :: [a]
pattern Nil = []

pattern Cons :: a -> [a] -> [a]
pattern Cons x xs = x : xs

type Tuple = (,)

pattern Tuple :: a -> b -> (a, b)
pattern Tuple a b = (a, b)

type Effect = IO

type Unit = ()

main :: Effect Unit
main = do
  _ <- getLine
  ranked <- readScores
  _ <- getLine
  player <- readScores
  doSolve solve (RankedScores ranked) (PlayerScores player)
  where
    readScores = getLine >>= pure <<< map read <<< words

doSolve :: forall a b. (a -> b -> Climb Rank) -> a -> b -> Effect Unit
doSolve solver highscore player = traverse_ logShow (solver highscore player)
