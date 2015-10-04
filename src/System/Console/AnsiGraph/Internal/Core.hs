module System.Console.AnsiGraph.Internal.Core (
    AnsiColor (..)
  , AGSettings (..)
  , blue, pink, white
  , defaultScaling
  , graphDefaults
  , Coloring (..)
  , realColors
  , imagColors
  , colorSets
  , invert
  , setFG
  , setBG
  , lineClear
  , applyColor
  , withColoring
) where

import System.Console.ANSI

---- Basics ----

-- | ANSI colors are characterized by a 'Color' and a 'ColorIntensity'. This
--   data type holds one of each.
data AnsiColor = AnsiColor ColorIntensity Color deriving Show

-- | Record that holds graphing options.
data AGSettings =
  AGSettings
    {
      -- | Foreground color for real number component
      realColor :: AnsiColor
      -- | Foreground color for imaginary number component.
    , imagColor :: AnsiColor
      -- | Background color for real number component.
    , realBG    :: AnsiColor
      -- | Background color for imaginary number component.
    , imagBG    :: AnsiColor
    -- | Framerate in fps.
    , framerate :: Int
    -- | How to rescale the size of a vector before displaying it
    --   (which is not implemented yet).
    , scaling   :: (Int -> Int)
    }

blue   = AnsiColor Vivid Blue
pink   = AnsiColor Vivid Magenta
white  = AnsiColor Vivid White
yellow = AnsiColor Vivid Yellow

-- | Default scaling function.
defaultScaling :: Int -> Int
defaultScaling = min 80

graphDefaults = AGSettings blue pink white white 15 defaultScaling

-- | Holds two 'AnsiColor's representing foreground and background colors for display via ANSI.
data Coloring = Coloring AnsiColor AnsiColor deriving Show

-- | Projection retrieving foreground and background colors
--   for real number graphs in the form of a 'Coloring'.
realColors :: AGSettings -> Coloring
realColors sets = Coloring (realColor sets) (realBG sets)

-- | Projection retrieving foreground and background colors
--   for imaginary component of complex number graphs in the form of a 'Coloring'.
imagColors :: AGSettings -> Coloring
imagColors sets = Coloring (imagColor sets) (imagBG sets)

-- | Retrieves a pair of 'Coloring's for real and imaginary graph components respectively.
colorSets :: AGSettings -> (Coloring,Coloring)
colorSets s = (Coloring (realColor s) (realBG s), Coloring (imagColor s) (imagBG s))

-- | Swaps foreground and background colors within a 'Coloring'.
invert :: Coloring -> Coloring
invert (Coloring fg bg) = Coloring bg fg

-- | 'SGR' command to set the foreground to the specified 'AnsiColor'.
setFG :: AnsiColor -> SGR
setFG (AnsiColor ci c) = SetColor Foreground ci c

-- | 'SGR' command to set the background to the specified 'AnsiColor'.
setBG :: AnsiColor -> SGR
setBG (AnsiColor ci c) = SetColor Background ci c

-- | Clear any SGR color settings and then print a new line.
lineClear :: IO ()
lineClear = setSGR [Reset] >> putStrLn ""

-- | Apply both foreground and background color.
applyColor :: Coloring -> IO ()
applyColor (Coloring fg bg) = setSGR [setFG fg, setBG bg]

-- | Apply the supplied foreground and background coloring, then perform the supplied IO action,
--   before reverting color settings and printing a new line.
withColoring :: Coloring -> IO () -> IO ()
withColoring clr io = do
  applyColor clr
  io
  lineClear
