{-# LANGUAGE FlexibleInstances #-}

-- | This module reexports the main "System.Console.AnsiGraph" module, as well as
--   "System.Console.AnsiGraph.FlexInstances", providing FlexibleInstances which conveniently
--   eliminates the need for wrapper types when using the 'Graphable' class.
module System.Console.AnsiGraph.Flex (
    module System.Console.AnsiGraph.Internal.FlexInstances
  , module System.Console.AnsiGraph
) where


import System.Console.AnsiGraph
import System.Console.AnsiGraph.Internal.FlexInstances
