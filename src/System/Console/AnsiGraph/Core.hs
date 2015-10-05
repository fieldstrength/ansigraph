-- | This module provides the core functionality of the ansigraph package:
--   terminal-based graphing for vectors and matrices of real and complex numbers.
--
--   This is implemented via a 'Graphable' type class.
--
--   __Ansigraph is intended to be used in on of two ways:__
--
--   * __By importing "System.Console.Ansigraph"__.
--   This provides all the functionality we typically use, including the FlexibleInstances
--   extension which makes it easier to use graphing functions by allowing instances like
--   'Graphable [Double]'.
--
--
--   * __By directly importing "System.Console.Ansigraph.Core"__, which does not activate FlexibleInstances but
--   includes everything else provided by the other module. This just means you must use one of a
--   handful of newtype wrappers, namely: 'Graph', 'PosGraph', 'CGraph', 'Mat', 'CMat'.
--   These wrappers are also available from the standard module.
module System.Console.Ansigraph.Core (

  -- * Core Functionality
  -- *** The Graphable class
    Graphable (graphWith)
  , graph
  , animateWith
  , animate

  -- *** Graphing options
  , AGSettings (..)

  -- *** Default options
  , graphDefaults
  , defaultScaling
  , blue, pink, white

  -- *** ANSI data
  , AnsiColor (..)
  , Coloring (..)

-- *** ANSI helpers
  , realColors
  , imagColors
  , colorSets
  , invert
  , setFG
  , setBG
  , lineClear
  , applyColor
  , withColoring

  -- * Graphable wrapper types
  , Graph (..)
  , CGraph (..)
  , PosGraph (..)
  , Mat (..)
  , CMat (..)

  -- * Graphing
  -- *** Horizontal vector graphing
  , displayRV
  , displayCV
  , simpleRender
  , simpleRenderR

  -- *** Matrix graphing
  , matShow
  , displayMat
  , displayCMat

-- *** Simple (non-ANSI) graphing for strictly-positive data
  , posgraph
  , posanim

) where

import System.Console.Ansigraph.Internal.Core
import System.Console.Ansigraph.Internal.Horizontal
import System.Console.Ansigraph.Internal.Matrix

import System.Console.ANSI
import System.IO (hFlush,stdout)
import Control.Concurrent (threadDelay)
import Data.List (intersperse,transpose)
import Data.Complex (Complex,realPart,imagPart,magnitude)


-- | Things that ansigraph knows how to render at the terminal are
--   instances of this class.
class Graphable a where
  graphWith :: AGSettings -> a -> IO ()

-- | Invokes the 'Graphable' type class method 'graphWith' with the
--   default 'AGSettings' record, 'graphDefaults'.
graph :: Graphable a => a -> IO ()
graph = graphWith graphDefaults


---- IO / ANSI helpers ----

-- | Pauses for the specified number of µs.
pauseFor :: Int -> IO ()
pauseFor n = hFlush stdout >> threadDelay n

-- | 'pauseFor' some time interval, then clear screen and set the cursor at (0,0).
next :: Int -> IO ()
next n = do pauseFor n
            clearScreen
            setCursorPosition 0 0

-- | For some number of frames per second, return the corresponding time delta in microseconds.
deltaFromFPS :: Int -> Int
deltaFromFPS fps = 1000000 `div` fps


---- Animation ----

-- | Any list of a 'Graphable' type can be made into an animation, by
--   'graph'ing each element with a time delay and screen-clear after each.
--   'AGSettings' are used to determine the time delta and any coloring/scaling options.
animateWith :: Graphable a => AGSettings -> [a] -> IO ()
animateWith s xs = let delta = deltaFromFPS $ framerate s in
  sequence_ $ intersperse (next delta) (graphWith s <$> xs)

-- | Perform 'animateWith' using default options. Equivalent to 'graph'ing each member
--   of the supplied list with a short delay and screen-clear after each.
animate :: Graphable a => [a] -> IO ()
animate = animateWith graphDefaults


---- Wrappers to avoid needing FlexibleInstances ----

-- | Wrapper type for graph of a real vector/function
newtype Graph = Graph { unGraph :: [Double] }

-- | Wrapper type for graph of a complex vector/function
newtype CGraph = CGraph { unCGraph :: [Complex Double] }

-- | Wrapper type for graph of a non-negative real vector/function
newtype PosGraph = PosGraph { unPosGraph :: [Double] }

-- | Wrapper type for graph of a real two-index vector/two-argument function
newtype Mat = Mat { unMat :: [[Double]] }

-- | Wrapper type for graph of a complex two-index vector/two-argument function
newtype CMat = CMat { unCMat :: [[Complex Double]] }


instance Graphable Graph where
  graphWith s = displayRV s . unGraph

instance Graphable CGraph where
  graphWith s = displayCV s . unCGraph

instance Graphable PosGraph where
  graphWith s = withColoring (realColors s) . putStrLn . simpleRender . unPosGraph

instance Graphable Mat where
  graphWith s = displayMat s . unMat

instance Graphable CMat where
  graphWith s = displayCMat s . unCMat


---- helpers for graphing/animating strictly-positive real functions ----

-- | Display a graph of the supplied (non-negative) real vector.
posgraph :: [Double] -> IO ()
posgraph = graph . PosGraph

-- | Display an animation of the supplied list of (non-negative) real vectors.
posanim :: [[Double]] -> IO ()
posanim = animate . map PosGraph
