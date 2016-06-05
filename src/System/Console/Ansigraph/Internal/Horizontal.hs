-- | Functionality for graphing 1-dimensional vectors.
module System.Console.Ansigraph.Internal.Horizontal (
    displayRV
  , displayCV
  , displayPV
  , renderPV
  , renderRV
  , renderCV
) where

import System.Console.Ansigraph.Internal.Core

import Data.Complex

---- Graphing Infrastructure  ----

barChars = "█▇▆▅▄▃▂▁ "

-- These values delineate regions for rounding to the nearest 1/8.
barVals :: [Double]
barVals = (+ 0.0625) . (/8) <$> [7,6..0]
     -- = [15/16, 13/16, 11/16, 9/16, 7/16, 5/16, 3/16, 1/16]

{- forward and reverse versions of unicode bar selection
   for positive and negative graph regions respectively -}

bars, barsR :: [(Double,Char)]
bars  = zip barVals barChars

barsR = zip barVals (reverse barChars)


selectBar, selectBarR :: Double -> Char
selectBar x = let l = filter (\p -> fst p < x) bars in
  case l of
       []     -> ' '
       (p:_) -> snd p

selectBarR x = let l = filter (\p -> fst p < x) barsR in
  case l of
       []     -> '█'
       (p:_) -> snd p


-- | Simple vector to String rendering that assumes positive input. Yields String of Unicode chars
--   representing graph bars varying in units of 1/8. The IO 'display' functions are preferable
--   for most use cases.
renderPV :: [Double] -> String
renderPV xs = let mx = maximum (filter (>= 0) $ 0:xs) in
              (selectBar . (/mx)) <$> xs

-- | Simple real vector rendering as a pair of strings. The IO 'display' functions are
--   preferable for most use cases.
renderRV :: [Double] -> (String,String)
renderRV l = let rp = l
                 rm = negate <$> rp
                 mx = maximum $ rp ++ rm
  in (selectBar  . (/mx) <$> rp,
      selectBarR . (/mx) <$> rm)

-- | Simple complex vector rendering as a pair of strings. The IO 'display' functions are
--   preferable for most use cases.
renderCV :: [Complex Double] -> (String,String,String,String)
renderCV l = let rp = realPart <$> l
                 rm = negate   <$> rp
                 ip = imagPart <$> l
                 im = negate   <$> ip
                 mx = maximum $ rp ++ rm ++ ip ++ im
  in (selectBar  . (/mx) <$> rp,
      selectBarR . (/mx) <$> rm,
      selectBar  . (/mx) <$> ip,
      selectBarR . (/mx) <$> im)


-- | ANSI based display for positive real vectors. Primarily invoked via 'graph', 'graphWith',
--   'animate', 'animateWith'.
displayPV :: GraphSettings -> [Double] -> IO ()
displayPV s l = let (rp,_) = renderRV l
                    rcol   = realColors s in colorStrLn rcol rp

-- | ANSI based display for real vectors. Primarily invoked via 'graph', 'graphWith',
--   'animate', 'animateWith'.
displayRV :: GraphSettings -> [Double] -> IO ()
displayRV s l = let (rp,rm) = renderRV l
                    rcol    = realColors s
  in do colorStrLn rcol          rp
        colorStrLn (invert rcol) rm

-- | ANSI based display for complex vectors. Primarily invoked via 'graph', 'graphWith',
--   'animate', 'animateWith'.
displayCV :: GraphSettings -> [Complex Double] -> IO ()
displayCV s l = let (rp,rm,ip,im) = renderCV l
                    (rcol,icol)   = colorSets s
  in do colorStrLn rcol          rp
        colorStrLn (invert rcol) rm
        colorStrLn icol          ip
        colorStrLn (invert icol) im
