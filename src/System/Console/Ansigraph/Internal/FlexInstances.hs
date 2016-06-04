{-# LANGUAGE FlexibleInstances #-}

-- | 'Graphable' Instances for lists of real or complex floating point numbers.
--   Users FlexibleInstances.
module System.Console.Ansigraph.Internal.FlexInstances where

import System.Console.Ansigraph.Core

import Data.Complex


-- | 1-dimensional real vector graph.
instance Graphable [Double] where
  graphWith = displayRV
  graphHeight _ = 2

-- | 1-dimensional complex vector graph.
instance Graphable [Complex Double] where
  graphWith = displayCV
  graphHeight _ = 4

-- | 2-dimensional real matrix graph.
instance Graphable [[Double]] where
  graphWith = displayMat
  graphHeight = length

-- | 2-dimensional complex matrix graph.
instance Graphable [[Complex Double]] where
  graphWith = displayCMat
  graphHeight = length
