module System.Console.AnsiGraph.Internal.Matrix (
    matShow
  , displayMat
  , displayMatC
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


matShow :: [[Double]] -> [String]
matShow m = let mx = mmax m
            in  mmap (selectBlock . (/ mx)) m

displayMat :: AGSettings -> [[Double]] -> IO ()
displayMat s m = withColoring (realColors s) . mapM_ putStrLn $ matShow m

displayMatC :: AGSettings -> [[Complex Double]] -> IO ()
displayMatC s m = displayMat s $ mmap magnitude m
