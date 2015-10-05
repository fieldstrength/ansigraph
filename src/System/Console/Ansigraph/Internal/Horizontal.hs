module System.Console.Ansigraph.Internal.Horizontal (
    displayRV
  , displayCV
  , displayPV
  , simpleRender
  , simpleRenderR
) where

import System.Console.Ansigraph.Internal.Core
import Data.Complex
import System.IO (hFlush, stdout)

---- Graphing Infrastructure  ----

barChars = "█▇▆▅▄▃▂▁"

-- These values delineate regions for rounding to the nearest 1/8.
barVals :: [Double]
barVals = (+ 0.0625) . (/8) <$> [7,6..0]
     -- = [15/16, 13/16, 11/16, 9/16, 7/16, 5/16, 3/16, 1/16]

{- forward and reverse versions of unicode bar selection
   for positive and negative graph regions respectively -}

bars, barsR :: [(Double,Char)]
bars  = zipWith (,) barVals barChars

barsR = zipWith (,) barVals (reverse barChars)


selectBar, selectBarR :: Double -> Char
selectBar x = let l = filter (\p -> fst p < x) bars in case l of
  []     -> ' '
  (p:ss) -> snd p

selectBarR x = let l = filter (\p -> fst p < x) barsR in case l of
  []     -> '█'
  (p:ss) -> snd p

-- | Simple vector rendering. Yields a string of unicode chars representing graph bars
--   varying in units of 1/8. To be primarily invoked via 'graph', 'graphWith',
--   'animate', 'animateWith'.
simpleRender :: [Double] -> String
simpleRender xs = let mx = maximum xs in
                  (selectBar . (/mx)) <$> xs

-- | Simple vector rendering – inverted version. Rarely used directly.
--   It is needed to render negative graph regions.
simpleRenderR :: [Double] -> String
simpleRenderR xs = let mx = maximum xs in
                   (selectBarR . (/mx)) <$> xs


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

renderRV :: [Double] -> (String,String)
renderRV l = let rp = l
                 rm = negate <$> rp
                 mx = maximum $ rp ++ rm
  in (selectBar  . (/mx) <$> rp,
      selectBarR . (/mx) <$> rm)

-- | ANSI based display for complex vectors. To be primarily invoked via 'graph', 'graphWith',
--   'animate', 'animateWith'.
displayCV :: AGSettings -> [Complex Double] -> IO ()
displayCV s l = let (rp,rm,ip,im) = renderCV l
                    (rcol,icol) = colorSets s
  in do colorStrLn rcol          rp
        colorStrLn (invert rcol) rm
        colorStrLn icol          ip
        colorStrLn (invert icol) im

-- | ANSI based display for real vectors. To be primarily invoked via 'graph', 'graphWith',
--   'animate', 'animateWith'.
displayRV :: AGSettings -> [Double] -> IO ()
displayRV s l = let (rp,rm) = renderRV l
                    rcol = realColors s
  in do colorStrLn rcol          rp
        colorStrLn (invert rcol) rm

-- | ANSI based display for positive real vectors. To be primarily invoked via 'graph', 'graphWith',
--   'animate', 'animateWith'.
displayPV :: AGSettings -> [Double] -> IO ()
displayPV s l = let (rp,_) = renderRV l
                    rcol = realColors s in colorStrLn rcol rp
