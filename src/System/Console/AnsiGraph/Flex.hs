{-# LANGUAGE FlexibleInstances #-}

module System.Console.AnsiGraph.Flex (
    module System.Console.AnsiGraph.Flex
  , module System.Console.AnsiGraph
) where


import Data.Complex

import System.Console.AnsiGraph


instance Graphable [Double] where
  graphWith = displayRV

instance Graphable [Complex Double] where
  graphWith = displayCV

instance Graphable [[Double]] where
  graphWith = displayMat

instance Graphable [[Complex Double]] where
  graphWith = displayMatC

{-}


instance Graphable PosGraph where
  graphWith s = let n = vertScale s in mapM_ putStrLn . multiRender n . unPosGraph

instance Graphable Graph where
  graphWith s = displayRV s . unGraph

instance Graphable GraphC where
  graphWith s = displayCV s . unGraphC

instance Graphable VectGraph where
  graphWith s = displayRV s . toList . unVectGraph

instance Graphable VectGraphC where
  graphWith s = displayCV s . toList . unVectGraphC -}
