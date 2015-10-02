module System.Console.AnsiGraph.Internal.Horizontal (
    displayRV
  , displayCV
  , simpleRender
  , simpleRenderR
) where

import System.Console.AnsiGraph.Internal.Core

import Data.Complex

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


simpleRender, simpleRenderR :: [Double] -> String
simpleRender xs = let mx = maximum xs in
                  (selectBar . (/mx)) <$> xs

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


displayCV :: AGSettings -> [Complex Double] -> IO ()
displayCV s l = let (rp,rm,ip,im) = renderCV l
                    (rcol,icol) = colorSets s
  in do withColoring rcol          $ putStr rp
        withColoring (invert rcol) $ putStr rm
        withColoring icol          $ putStr ip
        withColoring (invert icol) $ putStr im

displayRV :: AGSettings -> [Double] -> IO ()
displayRV s l = let (rp,rm) = renderRV l
                    rcol = realColors s
  in do withColoring rcol          $ putStr rp
        withColoring (invert rcol) $ putStr rm
