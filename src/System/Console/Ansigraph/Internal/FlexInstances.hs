{-# LANGUAGE FlexibleInstances #-}

module System.Console.Ansigraph.Internal.FlexInstances where

import System.Console.Ansigraph.Core
import Data.Complex


instance Graphable [Double] where
  graphWith = displayRV
  graphHeight _ = 2

instance Graphable [Complex Double] where
  graphWith = displayCV
  graphHeight _ = 4

instance Graphable [[Double]] where
  graphWith = displayMat
  graphHeight = length

instance Graphable [[Complex Double]] where
  graphWith = displayCMat
  graphHeight = length
