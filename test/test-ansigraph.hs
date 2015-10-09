module Main where

import Test.Hspec
import Test.QuickCheck
import System.Console.Ansigraph
import System.Console.Ansigraph.Examples (wave)
import Data.Complex (realPart)



---- For horizontal graphing tests ----

bars = " ▁▂▃▄▅▆▇█"

eighths :: [Double]
eighths = (/8) <$> [0..8]

{- These two vectors have a maximum element of 8, to maintain a particular normalization,
   while displacing other entries by just under one half. Both vectors should display
   as █▇▆▅▄▃▂▁. Same idea for m1, m_hi, m_low below. -}

v_hi :: [Double]
v_hi = 8 : map (+0.48) (reverse [0..7])

v_low :: [Double]
v_low = 8 : map (\x -> x - 0.48) (reverse [0..7])

{- These functions are for testing the property that:
   simpleRender v == complement <$> simpleRenderR v-}

match :: Eq a => a -> [a] -> [a] -> a
match x (y:ys) (z:zs) = if x == y then z else match x ys zs

complement :: Char -> Char
complement c = match c bars (reverse bars)

rwave = realPart <$> wave

barInvert (x,y) = (complement <$> y, complement <$> x)

---- For matrix graphing tests ----

densities = "░▒▓█"

fourths :: [Double]
fourths = (/4) <$> [1..4]

mmap :: (a -> b) -> [[a]] -> [[b]]
mmap = map . map

m1 :: [[Double]]
m1 = [[1,2,3],[3,4,4]]

m_low :: [[Double]]
m_low = [[0.55,1.55,2.55], [2.55,3.55,4]]

m_hi :: [[Double]]
m_hi = [[1.45,2.45,3.45], [3.45,4,4]]


---- The spec ----

main :: IO ()
main = hspec $ do
  describe "renderPV" $ do
    it "maps exact multiples of 1/8 to the correct characters" $ do
      renderPV eighths `shouldBe` bars

    it "maps scaled multiples of 1/8 to the correct characters" $ do
      renderPV ((* 1337) <$> eighths) `shouldBe` bars

    it "maps values near tops of rounding regions to the correct characters" $ do
      renderPV v_hi `shouldBe` (reverse bars)

    it "maps values near bottoms of rounding regions to the correct characters" $ do
      renderPV v_low `shouldBe` (reverse bars)

    it "maps zero vectors to whitespace" $ do
      renderPV [0,0,0] `shouldBe` "   "

    it "maps non-positive vectors to whitespace" $ do
      renderPV [-1,-23,-0.02] `shouldBe` "   "

    it "maps empty vectors to empty strings" $ do
      renderPV [] `shouldBe` ""



  describe "renderRV" $ do
    it "inverts rwave consistently" $ do
      let (p,n) = renderRV rwave
      renderRV (negate <$> rwave) `shouldBe` (complement <$> n, complement <$> p)
    it "inverts arbitrary vectors consistently" $ do
      property $ \xs -> renderRV xs == (barInvert $ renderRV $ negate <$> xs)


  describe "matShow" $ do
    it "maps exact multiples of 1/4 to the correct characters" $ do
      matShow [[1/4,2/4],[3/4,4/4]] `shouldBe` ["░▒", "▓█"]

    it "maps scaled multiples of 1/4 to the correct characters" $ do
      matShow (mmap (* 3097) [[1,2],[3,4]]) `shouldBe` ["░▒", "▓█"]
      matShow m1 `shouldBe` ["░▒▓", "▓██"]

    it "maps values near top of rounding regions to the correct characters" $ do
      matShow m_hi `shouldBe` ["░▒▓", "▓██"]

    it "maps values near bottom of rounding regions to the correct characters" $ do
      matShow m_low `shouldBe` ["░▒▓", "▓██"]
