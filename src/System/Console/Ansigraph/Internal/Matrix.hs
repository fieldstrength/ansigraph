-- | Functionality for graphing 2-dimensional matrices.
module System.Console.Ansigraph.Internal.Matrix (
    matShow
  , displayMat
  , displayCMat
) where


import System.Console.Ansigraph.Internal.Core

import Data.Complex
import Data.List (intersperse)
import Control.Monad.IO.Class (MonadIO, liftIO)


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
blocks = zip densityVals densityChars

data MatElement = MatElement !Bool {-# UNPACK #-} !Char

elemChar :: MatElement -> Char
elemChar (MatElement _ c) = c


putRealElement :: MonadIO m => GraphSettings -> MatElement -> m ()
putRealElement s (MatElement b c) = colorStr clring (c : " ")
  where clr    = if b then realNegColor s else realColor s
        clring = mkColoring clr (realBG s)

putImagElement :: MonadIO m => GraphSettings -> MatElement -> m ()
putImagElement s (MatElement b c) = colorStr clring $ c : " "
  where clr    = if b then imagNegColor s else imagColor s
        clring = mkColoring clr (imagBG s)

selectMatElement :: Double -> MatElement
selectMatElement x = let l = filter (\p -> fst p < abs x) blocks in case l of
  []    -> MatElement False   ' '
  (p:_) -> MatElement (x < 0) (snd p)


matElements :: [[Double]] -> [[MatElement]]
matElements m = let mx = mmax m
                in  mmap (selectMatElement . (/ mx)) m

-- | Given a matrix of Doubles, return the list of strings illustrating the absolute value
--   of each entry relative to the largest, via unicode chars that denote a particular density.
--   Used for testing purposes.
matShow :: [[Double]] -> [String]
matShow = mmap elemChar . matElements

newline = putStrLn' ""

intersperse' x l = intersperse x l ++ [x]


-- | Use ANSI coloring (specified by an 'GraphSettings') to visually display a Real matrix.
displayMat :: MonadIO m => GraphSettings -> [[Double]] -> m ()
displayMat s = liftIO . sequence_ . concat . intersperse' [newline] . displayRealMat s

-- | Use ANSI coloring (specified by a 'GraphSettings') to visually display a Real matrix.
displayRealMat :: MonadIO m => GraphSettings -> [[Double]] -> [[m ()]]
displayRealMat s = mmap (putRealElement s) . matElements

-- | Use ANSI coloring (specified by a 'GraphSettings') to visually display a Real matrix.
displayImagMat :: MonadIO m => GraphSettings -> [[Double]] -> [[m ()]]
displayImagMat s = mmap (putImagElement s) . matElements

-- | Use ANSI coloring (specified by an 'GraphSettings') to visually display a Complex matrix.
displayCMat :: MonadIO m => GraphSettings -> [[Complex Double]] -> m ()
displayCMat s m = liftIO . sequence_ . concat . intersperse' [newline] $
  zipWith (\xs ys -> xs ++ putStr " " : ys)
          (displayRealMat s $ mmap realPart m)
          (displayImagMat s $ mmap imagPart m)
