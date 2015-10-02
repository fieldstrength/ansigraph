module System.Console.AnsiGraph (
    module System.Console.AnsiGraph
  , module System.Console.AnsiGraph.Internal.Core
  , module System.Console.AnsiGraph.Internal.Horizontal
  , module System.Console.AnsiGraph.Internal.Matrix
) where

import System.Console.ANSI
import System.IO (hFlush,stdout)
import Control.Concurrent (threadDelay)
import Data.List (intersperse,transpose)
import Data.Complex (Complex,realPart,imagPart,magnitude)

import System.Console.AnsiGraph.Internal.Core
import System.Console.AnsiGraph.Internal.Horizontal
import System.Console.AnsiGraph.Internal.Matrix


class Graphable a where
  graphWith :: AGSettings -> a -> IO ()

graph :: Graphable a => a -> IO ()
graph = graphWith graphDefaults


---- IO / ANSI helpers ----

-- pause duration by µs
pauseFor :: Int -> IO ()
pauseFor n = hFlush stdout >> threadDelay n

next :: Int -> IO ()
next n = do pauseFor n
            clearScreen
            setCursorPosition 0 0

deltaFromFPS :: Int -> Int
deltaFromFPS fps = 1000000 `div` fps


---- Animation ----

animateWith :: Graphable a => AGSettings -> [a] -> IO ()
animateWith s xs = let delta = deltaFromFPS $ framerate s in
  sequence_ $ intersperse (next delta) (graphWith s <$> xs)

animate :: Graphable a => [a] -> IO ()
animate = animateWith graphDefaults


---- Wrappers to avoid needing FlexibleInstances
---- If you prefer the direct approach import AnsiGraph.Flex

newtype Graph = Graph { unGraph :: [Double] }

newtype ComGraph = ComGraph { unComGraph :: [Complex Double] }

newtype PosGraph = PosGraph { unPosGraph :: [Double] }

newtype Mat = Mat { unMat :: [[Double]] }



instance Graphable Mat where
  graphWith s = displayMat s . unMat

instance Graphable Graph where
  graphWith s = displayRV s . unGraph

instance Graphable ComGraph where
  graphWith s = displayCV s . unComGraph

instance Graphable PosGraph where
  graphWith s = withColoring (realColors s) . putStrLn . simpleRender . unPosGraph


-- helpers for graphing/animating strictly-positive real functions
posgraph :: [Double] -> IO ()
posgraph = graph . PosGraph

posanim :: [[Double]] -> IO ()
posanim = animate . map PosGraph
