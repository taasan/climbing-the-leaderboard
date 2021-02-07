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
--{-# LANGUAGE StandaloneKindSignatures #-}
--{-# LANGUAGE TypeOperators #-}
--{-# LANGUAGE TypeSynonymInstances #-}
--{-# LANGUAGE UndecidableInstances #-}
--{-# LANGUAGE UndecidableSuperClasses #-}
--{-# LANGUAGE UnicodeSyntax #-}

-- So we can pattern match on Tuple and Nil
{-# LANGUAGE PatternSynonyms #-}

{-# LANGUAGE CPP #-}

module PsPrelude
  ( module PsPrelude
  , module Prelude
  )
where

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

import Data.Foldable
  ( foldl'
  )

foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
foldl = foldl'

map :: Functor f => (a -> b) -> f a -> f b
map = fmap

identity :: a -> a
identity = id

type List = []

pattern Nil :: [a]
pattern Nil = []

pattern Cons :: a -> [a] -> [a]
pattern Cons x xs = x : xs

-- {-# COMPLETE Nil, Cons #-}

type Tuple = (,)

pattern Tuple :: a -> b -> (a, b)
pattern Tuple a b = (a, b)

type Effect = IO

type Unit = ()
