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
  , GraphSettings (..)

  -- **** Default options
  , graphDefaults
  , blue, pink, white, red, green
  , noColoring

  -- *** ANSI data
  -- **** Basic types from ANSI package
  , Color (..)
  , ColorIntensity (..)


-- **** Custom composite data types
  , AnsiColor (..)
  , Coloring (..)

-- *** ANSI helpers
  , mkColoring
  , fromFG
  , fromBG
  , realColors
  , imagColors
  , colorSets
  , invert
  , interpAnsiColor
  , setColor
  , clear
  , clearLn
  , applyColoring
  , colorStr
  , colorStrLn
  , boldStr
  , boldStrLn

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
  , posGraph
  , posAnim

-- *** For clearing
  , clearBack

) where

import System.Console.Ansigraph.Internal.Core
import System.Console.Ansigraph.Internal.Horizontal
import System.Console.Ansigraph.Internal.Matrix

import System.Console.ANSI
import Control.Concurrent     (threadDelay)
import Control.Monad          (replicateM_)
import Data.Complex           (Complex)
import Control.Monad.IO.Class (MonadIO, liftIO)


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
  graphWith :: MonadIO m => GraphSettings -> a -> m ()

  -- | The number of vertical lines a graph occupies.
  graphHeight :: a -> Int

-- | Invokes the 'Graphable' type class method 'graphWith' with the
--   default 'GraphSettings' record, 'graphDefaults'.
graph :: MonadIO m => Graphable a => a -> m ()
graph = graphWith graphDefaults


---- IO / ANSI helpers ----

-- | Clear the last @n@ lines of terminal text. Used to make graph animations. Rexported as
--   a handy convenience for other uses.
clearBack :: MonadIO m => Int -> m ()
clearBack n = do
  putStr' "\r"  -- return cursor to horizontal position 0
  replicateM_ n (liftIO $ cursorUpLine 1 >> clearLine)

-- | For some number of frames per second, return the corresponding time delta in microseconds.
deltaFromFPS :: Int -> Int
deltaFromFPS fps = 1000000 `div` fps


---- Animation ----

clearGraph :: MonadIO m => Graphable a => a -> m ()
clearGraph = clearBack . graphHeight

animationFrame :: MonadIO m => Graphable a => GraphSettings -> a -> m ()
animationFrame s x = do
  graphWith s x
  liftIO . threadDelay . deltaFromFPS . framerate $ s
  clearGraph x

-- | Any list of a 'Graphable' type can be made into an animation, by
--   'graph'ing each element with a time delay and screen-clear after each.
--   'GraphSettings' are used to determine the time delta and any coloring/scaling options.
animateWith :: MonadIO m => Graphable a => GraphSettings -> [a] -> m ()
animateWith _ []       = return ()
animateWith s [x]      = graphWith s x
animateWith s (x:y:zs) = animationFrame s x >> animateWith s (y:zs)

-- | Perform 'animateWith' using default options. Equivalent to 'graph'ing each member
--   of the supplied list with a short delay and screen-clear after each.
animate :: MonadIO m => Graphable a => [a] -> m ()
animate = animateWith graphDefaults

-- | Like 'animateWith', only it does not leave the final frame of the animation visible.
transientAnimWith :: MonadIO m => Graphable a => GraphSettings -> [a] -> m ()
transientAnimWith = mapM_ . animationFrame

-- | Like 'animate', only it does not leave the final frame of the animation visible.
transientAnim :: (MonadIO m, Graphable a) => [a] -> m ()
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
posGraph :: MonadIO m => [Double] -> m ()
posGraph = graph . PosGraph

-- | Display an animation of the supplied list of (non-negative) real vectors.
posAnim :: MonadIO m => [[Double]] -> m ()
posAnim = animate . map PosGraph
