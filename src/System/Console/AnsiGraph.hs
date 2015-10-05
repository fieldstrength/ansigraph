{-# LANGUAGE FlexibleInstances #-}

-- | This is the primary module to import for use of the ansigraph package, which provides
--   terminal-based graphing for vectors and matrices of real and complex numbers.
--
--   This functionality is implemented via a 'Graphable' type class.
--
--   __Ansigraph is intended to be used in on of two ways:__
--
--   * __By importing "System.Console.AnsiGraph"__.
--   This provides all the functionality we typically use, including the FlexibleInstances
--   extension which makes it easier to use graphing functions by allowing instances like
--   'Graphable [Double]'. It also provides "System.Console.AnsiGraph.Core" which provides all the
--   core functionality. See the Haddock page for that module for more details.
--
--   * __By directly importing "System.Console.AnsiGraph.Core"__, which does not activate
--   FlexibleInstances but includes everything else provided by the other module. This just means
--   you must use one of a handful of newtype wrappers, namely: 'Graph', 'PosGraph', 'CGraph',
--   'Mat', 'CMat'. These wrappers are also available from the standard module.
module System.Console.Ansigraph (
    module System.Console.Ansigraph.Internal.FlexInstances
  , module System.Console.Ansigraph.Core
) where


import System.Console.Ansigraph.Core
import System.Console.Ansigraph.Internal.FlexInstances
