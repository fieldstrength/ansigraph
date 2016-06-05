-- | A module that exports some simple demonstrations of how to use the package.
module System.Console.Ansigraph.Examples (
    waveDemo
  , waveDemoR
  , waveDemoP
  , matDemo
  , showColors
  , demo
  , wave
  , unitary
) where

import System.Console.Ansigraph

import System.Console.ANSI
import Control.Monad (forM_)
import Data.Complex  (Complex (..), cis, realPart)


---- Wave Demo ----

-- | A complex wave vector, part of the graph of /z(x,t) = exp(ix - it)/ in some units.
wave :: [Complex Double]
wave = cis . (*) (pi/20) <$> [0..79]

deltas :: [Double]
deltas = (*) (-pi/10) <$> [0..100]

waves :: [[Complex Double]]
waves = zipWith (\z -> map (* z)) (cis <$> deltas) $ repeat wave

rwaves :: [[Double]]
rwaves = map (map realPart) waves

pwaves :: [PosGraph]
pwaves = PosGraph . map (+1) <$> rwaves

-- | Display an animation of the positive real function /p(x,t) = cos(x-t) + 1/ in some units.
waveDemoP :: IO ()
waveDemoP = animate pwaves

-- | Display an animation of the real function /r(x,t) = cos(x-t)/ in the standard style, i.e. with both
--   positive and negative regions.
waveDemoR :: IO ()
waveDemoR = animate rwaves

-- | Display an animation of the complex wave /z(x,t) = exp(ix - it)/ in some units.
waveDemo :: IO ()
waveDemo = animate waves


---- Matrix Demo ----

vscale :: Num a => a -> [a] -> [a]
vscale x = map (* x)

mscale :: Num a => a -> [[a]] -> [[a]]
mscale x = map $ map (* x)

sx, sz, sI :: [[Double]]
sz  = [[1,0],[0,-1]]
sx  = [[0,1],[1,0]]
sI  = [[1,0],[0,1]]
-- isy = [[0,1],[-1,0]]

-- Time-exponentials of pauli matrices
-- exp(itσ) = cos(t)I + i sin(t)σ
sinSX, sinSZ, unitary :: Double -> [[Complex Double]]
sinSX   t = fromRealMs (mscale (cos t) sI) (mscale (sin t) sx)
sinSZ   t = fromRealMs (mscale (cos t) sI) (mscale (sin t) sz)

-- The following functions form a quick implementation of the matrix tensor product ('mox').
-- The details are not really necessary or relevant to use the library, only to make our example.

fromRealVs :: [Double] -> [Double] -> [Complex Double]
fromRealVs = zipWith (:+)

fromRealMs :: [[Double]] -> [[Double]] -> [[Complex Double]]
fromRealMs = zipWith fromRealVs

vox :: Num a => [a] -> [a] -> [a]
vox v w = concat $ map (flip vscale w) v

stepOne, stepTwo, mox :: Num a => [[a]] -> [[a]] -> [[a]]
stepOne m1 m2 = concat $ map (replicate (length m2)) m1
stepTwo m1 m2 = concat $ replicate (length m1) m2
mox     m1 m2 = zipWith vox (stepOne m1 m2) (stepTwo m1 m2)

-- | An example of a time-dependent matrix
unitary t = sinSZ t `mox` sinSX (2*t)

slowDeltas :: [Double]
slowDeltas = (*) (pi/50) <$> [0..100]

-- | Shows an animation of an example time-dependent matrix formed from Pauli matrices, called
--   'unitary'. Specifically, it is the tensor product of σt and σx exponentiated with different
--   frequencies.
matDemo :: IO ()
matDemo = animate $ unitary <$> slowDeltas


---- Show ANSI Colors ----

colors = [Black,Red,Green,Yellow,Blue,Magenta,Cyan,White]
intensities = [Dull,Vivid]

ansicolors :: [AnsiColor]
ansicolors = [ AnsiColor i c | c <- colors, i <- intensities ]

-- | Show all of the available 'AnsiColor's and corresponding 'ColorIntensity', 'Color' pairs.
showColors = do
  putStrLn "Available colors:"
  forM_ ansicolors $ \c -> do
    let clr = Coloring Nothing (Just c)
    colorStr clr $ replicate 20 ' '
    putStrLn $ "  " ++ show (intensity c) ++ " " ++ show (color c)
  setSGR [Reset]

cb, bc :: Coloring
cb = mkColoring (AnsiColor Dull Black) (AnsiColor Vivid Cyan)
bc = invert cb

newline = putStrLn ""

verticalPad io = do
  newline
  io
  newline
  newline


-- | Run all of the demos in sequence.
demo = do

  verticalPad $ boldStrLn cb "     Ansigraph demo     "

  putStr "Positive function graph  "
  colorStrLn bc " cos (x - t) + 1 "
  verticalPad waveDemoP

  putStr "Real function graph  "
  colorStrLn bc "  cos (x - t)  "
  verticalPad waveDemoR

  putStr "Complex function graph  "
  colorStrLn bc "  exp (ix - it)  "
  verticalPad waveDemo

  putStr "Complex matrix graph  "
  colorStrLn bc "  exp (it σz) ⊗ exp (2it σx)  "
  verticalPad matDemo

  showColors
  newline
