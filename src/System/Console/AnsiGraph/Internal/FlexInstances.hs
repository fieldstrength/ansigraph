{-# LANGUAGE FlexibleInstances #-}

-- | A module exporting instances of the 'Graphable' class that rely on
--   the FlexibleInstances extension. Intended to be used by importing
--   the "System.Console.AnsiGraph.Flex" module.
module System.Console.AnsiGraph.Internal.FlexInstances where

import Data.Complex

import System.Console.AnsiGraph


instance Graphable [Double] where
  graphWith = displayRV

instance Graphable [Complex Double] where
  graphWith = displayCV

instance Graphable [[Double]] where
  graphWith = displayMat

instance Graphable [[Complex Double]] where
  graphWith = displayCMat
