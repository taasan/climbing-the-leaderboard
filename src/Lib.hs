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
--{-# LANGUAGE PatternSynonyms #-}

-- For Read and Show instances
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE CPP #-}

module Lib
  ( module Lib
  , succ
  , tail
  )
where

import Data.Coerce
  ( Coercible,
    coerce,
  )

import PsPrelude

#include "Lib.purs"

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

logShow :: Show a => a -> IO ()
logShow = print

log :: String -> IO ()
log = putStrLn

wrap :: Coercible a b => a -> b
wrap = coerce

unwrap :: Coercible a b => b -> a
unwrap = coerce
