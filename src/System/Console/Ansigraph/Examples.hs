-- | A module that exports some simple demonstrations of how to use the package.
module System.Console.Ansigraph.Examples (
    waveDemo
  , waveDemoR
  , waveDemoP
  , showColors
  , demo
  , wave
) where

import System.Console.Ansigraph
import System.Console.ANSI
import Control.Monad (forM_)
import System.IO (hFlush, stdout, hSetBuffering, BufferMode(..))
import Data.Complex


---- Wave Demo ----

-- | A complex wave vector, part of the graph of /z(x,t) = exp(ix - it)/ in some units.
wave :: [Complex Double]
wave = cis . (*) (pi/20) <$> [0..79]

deltas :: [Double]
deltas = (*) (-pi/10) <$> [0..90]

waves :: [[Complex Double]]
waves = zipWith (\z -> map (* z)) (cis <$> deltas) $ repeat wave

pwave :: PosGraph
pwave = PosGraph $ (+1) . realPart <$> wave

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


---- Show ANSI Colors ----

colors = [Black,Red,Green,Yellow,Blue,Magenta,Cyan,White]
intensities = [Dull,Vivid]

ansicolors = [ AnsiColor i c | c <- colors, i <- intensities ]

-- | Show all of the available 'AnsiColor's and corresponding 'ColorIntensity', 'Color' pairs.
showColors = do
  putStrLn "\n Available colors:"
  forM_ ansicolors $ \c -> do
    let clr = Coloring c (AnsiColor Dull Black)
    colorStr clr $ replicate 20 '█'
    putStrLn $ "  " ++ show c
  setSGR [Reset]


cb = Coloring (AnsiColor Dull Black) (AnsiColor Vivid Cyan)
bc = invert cb

newline = putStrLn ""

verticalPad io = do
  newline
  io
  newline
  newline

disableBuffering = hSetBuffering stdout NoBuffering

-- | Run all of the demos in sequence.
demo = do
  disableBuffering

  setSGR [SetConsoleIntensity BoldIntensity]
  verticalPad $ colorStrLn cb "     Ansigraph demo     "
  setSGR [SetConsoleIntensity NormalIntensity]

  putStr "Positive function graph  "
  colorStrLn bc " sin (x - t) + 1/2 "
  verticalPad waveDemoP

  putStr "Real function graph  "
  colorStrLn bc "  sin (x - t)  "
  verticalPad waveDemoR

  putStr "Complex function graph  "
  colorStrLn bc "  exp (ix - it)  "
  verticalPad waveDemo

  showColors
  newline
