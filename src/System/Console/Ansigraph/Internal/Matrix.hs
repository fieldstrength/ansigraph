module System.Console.Ansigraph.Internal.Matrix (
    matShow
  , displayMat
  , displayCMat
) where

import System.Console.Ansigraph.Internal.Core

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

blocks :: [(Double,Char)]
blocks  = zip densityVals densityChars


selectBlock :: Double -> Char
selectBlock x = let l = filter (\p -> fst p < abs x) blocks in case l of
  []    -> ' '
  (p:_) -> snd p

-- | Given a matrix of Doubles, return the list of strings illustrating the absolute value
--   of each entry relative to the largest, via unicode chars that denote a particular density.
matShow :: [[Double]] -> [String]
matShow m = let mx = mmax m
            in  mmap (selectBlock . (/ mx)) m

-- | Use ANSI coloring (specified by an 'AGSettings') to visually display a Real matrix.
displayMat :: AGSettings -> [[Double]] -> IO ()
displayMat s = mapM_ (colorStrLn (realColors s)) . matShow


matShow_Imag :: [[Complex Double]] -> [String]
matShow_Imag m = let mx = max (mmax $ mmap realPart m)
                              (mmax $ mmap imagPart m)
                 in  mmap (selectBlock . (/ mx) . imagPart) m

matShow_Real :: [[Complex Double]] -> [String]
matShow_Real m = let mx = max (mmax $ mmap realPart m)
                              (mmax $ mmap imagPart m)
                 in  mmap (selectBlock . (/ mx) . realPart) m

-- | Use ANSI coloring (specified by an 'AGSettings') to visually display a Complex matrix.
displayCMat :: AGSettings -> [[Complex Double]] -> IO ()
displayCMat s m = sequence_ $
  zipWith (\x y -> x >> putStr " " >> y)
          (colorStr   (realColors s) <$> matShow_Real m)
          (colorStrLn (imagColors s) <$> matShow_Imag m)
