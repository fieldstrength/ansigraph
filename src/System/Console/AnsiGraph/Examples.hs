-- | A module that exports some simple demonstrations of how to use the package.
module System.Console.AnsiGraph.Examples (
    waveDemo
  , waveDemoR
  , waveDemoP
  , showColors
  , demo
) where

import System.Console.AnsiGraph.Flex
import System.Console.ANSI
import Control.Monad (forM_)
import Data.Complex


---- Wave Demo ----

wave :: [Complex Double]
wave = cis . (*) (pi/20) <$> [0..79]

deltas :: [Double]
deltas = (*) (-pi/10) <$> [0..50]

waves :: [[Complex Double]]
waves = zipWith (\z -> map (* z)) (cis <$> deltas) $ replicate 50 wave

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

-- | Show all of the available 'AnsiColor's and corresponding 'ColorIntensity', 'Color' pairs.
showColors = do
  putStrLn "\n Available colors:"
  forM_ colors $ \clr -> do
     forM_ intensities $ \inten -> do
       setSGR [SetColor Foreground inten clr]
       putStr $ replicate 20 '█'
       setSGR [Reset]
       putStrLn $ "  " ++ show inten ++ " " ++ show clr
  setSGR [Reset]

-- | Run all of the demos in sequence.
demo = sequence_ [waveDemoP,waveDemoR,waveDemo,showColors]
