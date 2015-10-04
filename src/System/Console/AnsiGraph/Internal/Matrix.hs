module System.Console.AnsiGraph.Internal.Matrix (
    matShow
  , displayMat
  , displayCMat
) where

import System.Console.AnsiGraph.Internal.Core

import Data.Complex

---- Matrices ----

mmap :: (a -> b) -> [[a]] -> [[b]]
mmap = map . map

mmax :: (Num a, Ord a) => [[a]] -> a
mmax = maximum . map maximum . mmap abs

densityChars = "█▓▒░"

densityVals :: [Double]
densityVals = (+ 0.125) . (/4) <$> [3,2,1,0]
         -- = [7/8, 5/8, 3/8, 1/8]

blocks, blocksR :: [(Double,Char)]
blocks  = zipWith (,) densityVals densityChars

blocksR = zipWith (,) densityVals (reverse densityChars)


selectBlock :: Double -> Char
selectBlock x = let l = filter (\p -> fst p < abs x) blocks in case l of
  []     -> ' '
  (p:ss) -> snd p

-- | Given a matrix of Doubles, return the list of strings illustrating the absolute value
--   of each entry relative to the largest, via unicode chars that denote a particular density.
matShow :: [[Double]] -> [String]
matShow m = let mx = mmax m
            in  mmap (selectBlock . (/ mx)) m

-- | Use ANSI coloring (specified by an 'AGSettings') to visually display a Real matrix.
displayMat :: AGSettings -> [[Double]] -> IO ()
displayMat s m = withColoring (realColors s) . mapM_ putStrLn $ matShow m

-- | Use ANSI coloring (specified by an 'AGSettings') to visually display a Complex matrix.
displayCMat :: AGSettings -> [[Complex Double]] -> IO ()
displayCMat s m = displayMat s $ mmap magnitude m
