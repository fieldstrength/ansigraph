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
--   * __By directly importing "System.Console.Ansigraph.Core"__, which does not activate
--   FlexibleInstances but includes everything else provided by the other module. This just means
--   you must use one of a few newtype wrappers, namely: 'Graph', 'PosGraph', 'CGraph',
--   'Mat', 'CMat'. They are also available from the standard module.
module System.Console.Ansigraph.Core (

  -- * Core Functionality
  -- ** The Graphable class
    Graphable (..)
  , graph
  , animateWith
  , animate
  , transientAnim
  , transientAnimWith

  -- *** Graphing options
  , AGSettings (..)

  -- *** Default options
  , graphDefaults
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
  , clear
  , clearLn
  , applyColor
  , colorStr
  , colorStrLn

  -- * Graphable wrapper types
  , Graph (..)
  , CGraph (..)
  , PosGraph (..)
  , Mat (..)
  , CMat (..)

  -- * Graphing
  -- *** Horizontal vector graphing (IO actions)
  , displayPV
  , displayRV
  , displayCV

  -- *** Horizontal rendering logic (producing strings)
  , renderPV
  , renderRV
  , renderCV

  -- *** Matrix graphing
  , displayMat
  , displayCMat
  , matShow

-- *** Simple (non-ANSI) graphing for strictly-positive data
  , posgraph
  , posanim

) where

import System.Console.Ansigraph.Internal.Core
import System.Console.Ansigraph.Internal.Horizontal
import System.Console.Ansigraph.Internal.Matrix

import System.Console.ANSI
import System.IO          (hFlush,stdout)
import Control.Concurrent (threadDelay)
import Control.Monad      (replicateM_)
import Data.Complex       (Complex)


-- | Things that ansigraph knows how to render at the terminal are instances of this class.
--
--   In general, when ANSI codes are involved, a 'graphWith' method should fush stdout when
--   finished, and whenever codes are invoked to i.e. change terminal colors. This is easily
--   handled by defining it in terms of 'colorStr' and 'colorStrLn'.
--
--   The 'graphHeight' function specifies how many vertical lines a graph occupies and is
--   needed for animations to work properly
class Graphable a where

  -- | Render a graph to standard output.
  graphWith :: AGSettings -> a -> IO ()

  -- | The number of vertical lines a graph occupies.
  graphHeight :: a -> Int

-- | Invokes the 'Graphable' type class method 'graphWith' with the
--   default 'AGSettings' record, 'graphDefaults'.
graph :: Graphable a => a -> IO ()
graph = graphWith graphDefaults


---- IO / ANSI helpers ----

clearBack :: Int -> IO ()
clearBack n = do
  putStr "\r"  -- return cursor to horizontal position 0
  replicateM_ n (cursorUpLine 1 *> clearLine)

-- | For some number of frames per second, return the corresponding time delta in microseconds.
deltaFromFPS :: Int -> Int
deltaFromFPS fps = 1000000 `div` fps


---- Animation ----

clearGraph :: Graphable a => a -> IO ()
clearGraph = clearBack . graphHeight

animationFrame :: Graphable a => AGSettings -> a -> IO ()
animationFrame s x = do
  graphWith s x
  threadDelay . deltaFromFPS . framerate $ s
  clearGraph x

-- | Any list of a 'Graphable' type can be made into an animation, by
--   'graph'ing each element with a time delay and screen-clear after each.
--   'AGSettings' are used to determine the time delta and any coloring/scaling options.
animateWith :: Graphable a => AGSettings -> [a] -> IO ()
animateWith s []       = return ()
animateWith s [x]      = graphWith s x
animateWith s (x:y:zs) = animationFrame s x *> animateWith s (y:zs)

-- | Perform 'animateWith' using default options. Equivalent to 'graph'ing each member
--   of the supplied list with a short delay and screen-clear after each.
animate :: Graphable a => [a] -> IO ()
animate = animateWith graphDefaults

-- | Like 'animateWith', only it does not leave the final frame of the animation visible.
transientAnimWith :: Graphable a => AGSettings -> [a] -> IO ()
transientAnimWith = mapM_ . animationFrame

-- | Like 'animate', only it does not leave the final frame of the animation visible.
transientAnim :: Graphable a => [a] -> IO ()
transientAnim = transientAnimWith graphDefaults


---- Wrappers to avoid needing FlexibleInstances ----

-- | Wrapper type for graph of a real vector/function.
newtype Graph = Graph { unGraph :: [Double] }

-- | Wrapper type for graph of a complex vector/function.
newtype CGraph = CGraph { unCGraph :: [Complex Double] }

-- | Wrapper type for graph of a non-negative real vector/function.
newtype PosGraph = PosGraph { unPosGraph :: [Double] }

-- | Wrapper type for graph of a real two-index vector/two-argument function.
newtype Mat = Mat { unMat :: [[Double]] }

-- | Wrapper type for graph of a complex two-index vector/two-argument function.
newtype CMat = CMat { unCMat :: [[Complex Double]] }


instance Graphable Graph where
  graphWith s = displayRV s . unGraph
  graphHeight _ = 2

instance Graphable CGraph where
  graphWith s = displayCV s . unCGraph
  graphHeight _ = 4

instance Graphable PosGraph where
  graphWith s = displayPV s . unPosGraph
  graphHeight _ = 1

instance Graphable Mat where
  graphWith s = displayMat s . unMat
  graphHeight = length . unMat

instance Graphable CMat where
  graphWith s = displayCMat s . unCMat
  graphHeight = length . unCMat


---- helpers for graphing/animating strictly-positive real functions ----

-- | Display a graph of the supplied (non-negative) real vector.
posgraph :: [Double] -> IO ()
posgraph = graph . PosGraph

-- | Display an animation of the supplied list of (non-negative) real vectors.
posanim :: [[Double]] -> IO ()
posanim = animate . map PosGraph
