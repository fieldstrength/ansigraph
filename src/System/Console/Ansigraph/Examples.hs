-- | A module that shows some simple examples demonstrating how to use the package.
module System.Console.Ansigraph.Examples (

    demo
  , legend
  , showColors

-- * Horizontal graphs
  , waveDemoComplex
  , waveDemoReal
  , waveDemoPositive
  , wave

-- * Matrix graphs

  , matDemoReal
  , matDemoComplex
  , unitary

) where

import System.Console.Ansigraph

import System.Console.ANSI
import Control.Monad      (forM_)
import Data.Complex       (Complex (..), cis, realPart)
import Control.Concurrent (threadDelay)


---- Wave Demo ----

-- | A complex wave vector, part of the graph of /z(x,t) = exp(ix - it)/ in some units.
wave :: [Complex Double]
wave = cis . (*) (pi/20) <$> [0..79]

deltas :: [Double]
deltas = (*) (-pi/10) <$> [0..80]

waves :: [[Complex Double]]
waves = zipWith (\z -> map (* z)) (cis <$> deltas) $ repeat wave

rwaves :: [[Double]]
rwaves = map (map realPart) waves

pwaves :: [PosGraph]
pwaves = PosGraph . map (+1) <$> rwaves

-- | Display an animation of the positive real function /p(x,t) = cos(x-t) + 1/ in some units.
waveDemoPositive :: IO ()
waveDemoPositive = animate pwaves

-- | Display an animation of the real function /r(x,t) = cos(x-t)/ in the standard style, i.e. with both
--   positive and negative regions.
waveDemoReal :: IO ()
waveDemoReal = animate rwaves

-- | Display an animation of the complex wave /z(x,t) = exp(ix - it)/ in some units.
waveDemoComplex :: IO ()
waveDemoComplex = animate waves


---- Matrix Demos ----

vscale :: Num a => a -> [a] -> [a]
vscale x = map (* x)

mscale :: Num a => a -> [[a]] -> [[a]]
mscale x = map $ map (* x)


-- The following is a quick implementation of the matrix tensor product ('mox').
-- The details are not relevant to the library, but are only used for this example.

fromRealVs :: [Double] -> [Double] -> [Complex Double]
fromRealVs = zipWith (:+)

fromRealMs :: [[Double]] -> [[Double]] -> [[Complex Double]]
fromRealMs = zipWith fromRealVs

vox :: Num a => [a] -> [a] -> [a]
vox v w = concat $ map (flip vscale w) v

-- matrix tensor product
stepOne, stepTwo, mox :: Num a => [[a]] -> [[a]] -> [[a]]
stepOne m1 m2 = concat $ map (replicate (length m2)) m1
stepTwo m1 m2 = concat $ replicate (length m1) m2
mox     m1 m2 = zipWith vox (stepOne m1 m2) (stepTwo m1 m2)


---- Complex matrix example ----

sx, sz, sI :: [[Double]]
sz  = [[1,0],[0,-1]]
sx  = [[0,1],[1,0]]
sI  = [[1,0],[0,1]]

-- Time-exponentials of pauli matrices
-- exp(itσ) = cos(t)I + i sin(t)σ
sinSX, sinSZ :: Double -> [[Complex Double]]
sinSX t = fromRealMs (mscale (cos t) sI) (mscale (sin t) sx)
sinSZ t = fromRealMs (mscale (cos t) sI) (mscale (sin t) sz)

-- | An example of a time-dependent complex matrix.
unitary :: Double -> [[Complex Double]]
unitary t = sinSZ t `mox` sinSX (2*t)

slowDeltas :: [Double]
slowDeltas = (*) (pi/50) <$> [0..100]

-- | Shows an animation of an example time-dependent matrix formed from Pauli matrices, called
--   'unitary'. Specifically, it is the tensor product of σz and σx exponentiated with different
--   frequencies.
matDemoComplex :: IO ()
matDemoComplex = animate $ unitary <$> slowDeltas


---- Real matrix example ----

ry :: Double -> [[Double]]
ry t = [[cos t, 0, (sin t)]
       ,[0, 1, 0]
       ,[-(sin t),0,cos t]]

-- | An example real matrix animation.
matDemoReal :: IO ()
matDemoReal = animate $ (\t -> ry t `mox` ry (2*t)) <$> slowDeltas


---- Show ANSI Colors ----

colors = [Black,Red,Green,Yellow,Blue,Magenta,Cyan,White]
intensities = [Dull,Vivid]

ansicolors :: [AnsiColor]
ansicolors = [ AnsiColor i c | c <- colors, i <- intensities ]

-- | Show all of the available 'AnsiColor's with corresponding 'ColorIntensity', 'Color' pairs.
showColors = do
  boldStrLn noColoring "Available colors"
  newline
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

pause :: IO ()
pause = threadDelay 2000000

-- | Displays a legend showing color conventions for supported graph types.
legend = do
  boldStrLn cb "       Legend       "
  newline

  boldStrLn noColoring "Horizontal Graphs"
  newline
  colorStr (fromBG blue) "  "
  putStrLn " Real component (positive and negative)"
  newline
  colorStr (fromBG pink) "  "
  putStrLn " Imag component (positive and negative)"
  newline

  boldStrLn noColoring "Matrix Graphs"
  newline
  putStr "  "
  colorStr (mkColoring white pink) "+i"
  putStrLn "  "
  colorStr (mkColoring white red) "-r"
  putStr "  "
  colorStrLn (mkColoring white blue) "+r"
  putStr "  "
  colorStr (mkColoring white green) "-i"
  putStrLn "  "


-- | Run all of the demos in sequence.
demo = do

  verticalPad $ boldStrLn cb "       Ansigraph demo       "

  putStr "Positive function graph  "
  colorStrLn bc " cos (x - t) + 1 "
  verticalPad waveDemoPositive

  putStr "Real function graph  "
  colorStrLn bc "  cos (x - t)  "
  verticalPad waveDemoReal

  putStr "Complex function graph  "
  colorStrLn bc "  exp (ix - it)  "
  verticalPad waveDemoComplex

  putStr "Real matrix graph  "
  colorStrLn bc "  rotate_Y(t) ⊗ rotate_Y(2t)  "
  verticalPad matDemoReal

  putStr "Complex matrix graph  "
  colorStrLn bc "  exp (it σz) ⊗ exp (2it σx)  "
  verticalPad matDemoComplex

  verticalPad showColors

  pause

  verticalPad legend
