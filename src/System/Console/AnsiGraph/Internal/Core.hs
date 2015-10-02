module System.Console.AnsiGraph.Internal.Core where

import System.Console.ANSI

---- Basics ----

data AnsiColor = AnsiColor ColorIntensity Color deriving Show

data AGSettings =
  AGSettings
    { realColor :: AnsiColor
    , imagColor :: AnsiColor
    , realBG    :: AnsiColor
    , imagBG    :: AnsiColor
    , framerate :: Int
    , scaling   :: (Int -> Int)
    }

blue   = AnsiColor Vivid Blue
pink   = AnsiColor Vivid Magenta
white  = AnsiColor Vivid White
yellow = AnsiColor Vivid Yellow

defaultScaling :: Int -> Int
defaultScaling = min 80

graphDefaults = AGSettings blue pink white white 15 defaultScaling


data Coloring = Coloring AnsiColor AnsiColor deriving Show


realColors :: AGSettings -> Coloring
realColors sets = Coloring (realColor sets) (realBG sets)

imagColors :: AGSettings -> Coloring
imagColors sets = Coloring (imagColor sets) (imagBG sets)

colorSets :: AGSettings -> (Coloring,Coloring)
colorSets s = (Coloring (realColor s) (realBG s), Coloring (imagColor s) (imagBG s))

invert :: Coloring -> Coloring
invert (Coloring fg bg) = Coloring bg fg


setFG :: AnsiColor -> SGR
setFG (AnsiColor ci c) = SetColor Foreground ci c

setBG :: AnsiColor -> SGR
setBG (AnsiColor ci c) = SetColor Background ci c

lineClear :: IO ()
lineClear = setSGR [Reset] >> putStrLn ""

applyColor :: Coloring -> IO ()
applyColor (Coloring fg bg) = setSGR [setFG fg, setBG bg]

withColoring :: Coloring -> IO () -> IO ()
withColoring clr io = do
  applyColor clr
  io
  lineClear
