module Main where

import Test.Hspec
import Test.QuickCheck
import System.Console.Ansigraph
import System.Console.Ansigraph.Examples (wave)
import Data.Complex
import Data.Char (isSpace)


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

-- the 'complement' of a particular bar character is the one representing the negation of the
-- corresponding numeric value (when displayed with reversed foreground/background colors).
complement :: Char -> Char
complement c = match c bars (reverse bars)

match :: Eq a => a -> [a] -> [a] -> a
match x (y:ys) (z:zs) = if x == y then z else match x ys zs


rwave = realPart <$> wave

barInvert :: (String,String) -> (String,String)
barInvert (x,y)  = (complement <$> y, complement <$> x)

barInvertC :: (String,String,String,String) -> (String,String,String,String)
barInvertC (x1,y1,x2,y2) = (complement <$> y1,
                            complement <$> x1,
                            complement <$> y2,
                            complement <$> x2)

---- For matrix graphing tests ----

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

    it "maps exact multiples of 1/8 to the correct characters" $
      renderPV eighths `shouldBe` bars

    it "maps scaled multiples of 1/8 to the correct characters" $
      renderPV ((* 1337) <$> eighths) `shouldBe` bars

    it "maps values near tops of rounding regions to the correct characters" $
      renderPV v_hi `shouldBe` reverse bars

    it "maps values near bottoms of rounding regions to the correct characters" $
      renderPV v_low `shouldBe` reverse bars

    it "maps zero vectors to whitespace" $
      renderPV [0,0,0] `shouldBe` "   "

    it "maps non-positive vectors to whitespace" $
      renderPV [-1,-23,-0.02] `shouldBe` "   "

    it "maps null vectors to null strings" $
      renderPV [] `shouldBe` ""


  describe "renderRV" $ do

    it "inverts 'rwave' consistently" $
      let (p,n) = renderRV rwave
      in  renderRV (negate <$> rwave) `shouldBe` (complement <$> n, complement <$> p)

    it "inverts arbitrary vectors consistently" $
      property $ \xs -> renderRV xs == barInvert (renderRV $ negate <$> xs)

    it "maps arbitrary-dimensional zero vectors to whitespace, for positive components" $
      property $ \n -> let (p,m) = renderRV $ replicate n 0
                       in  all isSpace p

    it "maps arbitrary-dimensional zero vectors to solid bocks █, for negative components" $
      property $ \n -> let (p,m) = renderRV $ replicate n 0
                       in  all (== '█') m

  describe "renderCV" $ do

    it "inverts 'wave' consistently" $ do
      let (rp,rn,ip,im) = renderCV wave
      renderCV (negate <$> wave) `shouldBe` (complement <$> rn,
                                             complement <$> rp,
                                             complement <$> im,
                                             complement <$> ip)

    it "inverts arbitrary complex vectors consistently" $
      property $ \zs -> renderCV zs == barInvertC (renderCV $ negate <$> zs)

    it "maps arbitrary dimensional zero vectors to whitespace, for positive components" $
      property $ \n -> let (r1,_,i1,_) = renderCV $ replicate n (0.0 :+ 0.0)
                       in  all isSpace $ r1 ++ i1

    it "maps arbitrary dimensional zero vectors to solid bocks █, for negative components" $
      property $ \n -> let (_,r2,_,i2) = renderCV $ replicate n (0.0 :+ 0.0)
                       in  all (== '█') $ r2 ++ i2

    it "transforms appropriately under exchange of real and complex components" $
      property $ \v -> let v'  = (\(x :+ y) -> (y :+ x)) <$> v
                           (x1,y1,x2,y2)  = renderCV v
                           (x3,y3,x4,y4)  = renderCV v'
                       in  and [x1 == x4,
                                y1 == y4,
                                x2 == x3,
                                y2 == y3]

  describe "matShow" $ do

    it "maps exact multiples of 1/4 to the correct characters" $
      matShow [[1/4,2/4],[3/4,4/4]] `shouldBe` ["░▒", "▓█"]

    it "maps scaled multiples of 1/4 to the correct characters" $ do
      matShow (mmap (* 3097) [[1,2],[3,4]]) `shouldBe` ["░▒", "▓█"]
      matShow m1 `shouldBe` ["░▒▓", "▓██"]

    it "maps values near top of rounding regions to the correct characters" $
      matShow m_hi `shouldBe` ["░▒▓", "▓██"]

    it "maps values near bottom of rounding regions to the correct characters" $
      matShow m_low `shouldBe` ["░▒▓", "▓██"]
