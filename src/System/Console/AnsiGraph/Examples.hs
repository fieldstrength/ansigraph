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


pwave = PosGraph $ (+1) . realPart <$> wave

rwaves = map (map realPart) waves

pwaves = PosGraph . map (+1) <$> rwaves


waveDemo, waveDemoP, waveDemoR :: IO ()
waveDemoP = animate pwaves

waveDemoR = animate rwaves

waveDemo  = animate waves


---- Show ANSI Colors ----

colors = [Black,Red,Green,Yellow,Blue,Magenta,Cyan,White]
intensities = [Dull,Vivid]

showColors = do
  putStrLn "\n Available colors:"
  forM_ colors $ \clr -> do
     forM_ intensities $ \inten -> do
       setSGR [SetColor Foreground inten clr]
       putStr $ replicate 20 '█'
       setSGR [Reset]
       putStrLn $ "  " ++ show inten ++ " " ++ show clr
  setSGR [Reset]


demo = sequence_ [waveDemoP,waveDemoR,waveDemo,showColors]
