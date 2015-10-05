{-# LANGUAGE FlexibleInstances #-}

module System.Console.Ansigraph.Internal.FlexInstances where

import System.Console.Ansigraph.Core
import Data.Complex


instance Graphable [Double] where
  graphWith = displayRV

instance Graphable [Complex Double] where
  graphWith = displayCV

instance Graphable [[Double]] where
  graphWith = displayMat

instance Graphable [[Complex Double]] where
  graphWith = displayCMat
