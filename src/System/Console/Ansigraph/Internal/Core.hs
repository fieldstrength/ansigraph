-- | Core functionality of the package. Import from either "System.Console.Ansigraph" or
--   "System.Console.Ansigraph.Core".
module System.Console.Ansigraph.Internal.Core where

import System.Console.ANSI
import System.IO (hFlush, stdout)

---- Basics ----

-- | ANSI colors are characterized by a 'Color' and a 'ColorIntensity'. This
--   data type holds one of each.
data AnsiColor = AnsiColor ColorIntensity Color deriving Show

-- | Record that holds graphing options.
data GraphSettings =
  GraphSettings
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

    }


-- | 'Vivid' 'Blue' – used as the default real foreground color.
blue  = AnsiColor Vivid Blue

-- | 'Vivid' 'Magenta' – used as the default foreground color for imaginary
--   graph component.
pink  = AnsiColor Vivid Magenta

-- | 'Vivid' 'White' – used as the default graph background color
--   for both real and imaginary graph components.
white = AnsiColor Vivid White

-- | Default graph settings.
graphDefaults = GraphSettings blue pink white white 15

-- | Holds two 'AnsiColor's representing foreground and background colors for display via ANSI.
data Coloring = Coloring { foreground :: AnsiColor,
                           background :: AnsiColor } deriving Show

-- | Projection retrieving foreground and background colors
--   for real number graphs in the form of a 'Coloring'.
realColors :: GraphSettings -> Coloring
realColors sets = Coloring (realColor sets) (realBG sets)

-- | Projection retrieving foreground and background colors
--   for imaginary component of complex number graphs in the form of a 'Coloring'.
imagColors :: GraphSettings -> Coloring
imagColors sets = Coloring (imagColor sets) (imagBG sets)

-- | Retrieves a pair of 'Coloring's for real and imaginary graph components respectively.
colorSets :: GraphSettings -> (Coloring,Coloring)
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

-- | Clear any SGR settings and then flush stdout.
clear :: IO ()
clear = setSGR [Reset] *> hFlush stdout

-- | Clear any SGR settings, flush stdout and print a new line.
clearLn :: IO ()
clearLn = clear *> putStrLn ""

-- | Apply both foreground and background color.
applyColor :: Coloring -> IO ()
applyColor (Coloring fg bg) = setSGR [setFG fg, setBG bg]

-- | Use a particular ANSI 'Coloring' to print a string at the terminal (without a newline),
--   then clear all ANSI SGR codes and flush stdout.
colorStr :: Coloring -> String -> IO ()
colorStr c s = do
  applyColor c
  putStr s
  clear

-- | Use a particular ANSI 'Coloring' to print a string at the terminal,
--   then clear all ANSI SGR codes, print a newline and flush stdout.
colorStrLn :: Coloring -> String -> IO ()
colorStrLn c s = do
  applyColor c
  putStr s
  clearLn
