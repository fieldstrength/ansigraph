-- | Core functionality of the package. Import from either "System.Console.Ansigraph" or
--   "System.Console.Ansigraph.Core".
module System.Console.Ansigraph.Internal.Core where

import System.Console.ANSI
import System.IO (hFlush, stdout)

---- Basics ----

-- | ANSI colors are characterized by a 'Color' and a 'ColorIntensity'.
data AnsiColor = AnsiColor {
    intensity :: ColorIntensity
  , color     :: Color
  } deriving Show

-- | Record that holds graphing options.
data GraphSettings = GraphSettings {

      -- | Foreground color for real number component.
      realColor :: AnsiColor
      -- | Foreground color for imaginary number component.
    , imagColor :: AnsiColor
      -- | Foreground color for negative real values. For matrix graphs only.
    , realNegColor :: AnsiColor
      -- | Foreground color for negative imaginary values. For matrix graphs only.
    , imagNegColor :: AnsiColor
      -- | Background color for real number component.
    , realBG :: AnsiColor
      -- | Background color for imaginary number component.
    , imagBG :: AnsiColor
    -- | Framerate in FPS.
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

-- | 'Vivid' 'Red' – used as the default foreground color for negative real component.
red   = AnsiColor Vivid Red

-- | 'Dull' 'Green' – used as the default foreground color for negative imaginary component.
green = AnsiColor Dull Green

-- | Default graph settings.
graphDefaults = GraphSettings blue pink red green white white 15

-- | Holds two 'Maybe' 'AnsiColor's representing foreground and background colors for display via ANSI.
--   'Nothing' means use the default terminal color.
data Coloring = Coloring { foreground :: Maybe AnsiColor,
                           background :: Maybe AnsiColor } deriving Show

-- | A 'Coloring' representing default terminal colors, i.e. two 'Nothing's.
noColoring = Coloring Nothing Nothing

-- | Helper constructor function for 'Coloring' that takes straight 'AnsiColor's without 'Maybe'.
mkColoring :: AnsiColor -> AnsiColor -> Coloring
mkColoring c1 c2 = Coloring (Just c1) (Just c2)

-- | Projection retrieving foreground and background colors
--   for real number graphs in the form of a 'Coloring'.
realColors :: GraphSettings -> Coloring
realColors s = mkColoring (realColor s) (realBG s)

-- | Projection retrieving foreground and background colors
--   for imaginary component of complex number graphs in the form of a 'Coloring'.
imagColors :: GraphSettings -> Coloring
imagColors s = mkColoring (imagColor s) (imagBG s)

-- | Retrieves a pair of 'Coloring's for real and imaginary graph components respectively.
colorSets :: GraphSettings -> (Coloring,Coloring)
colorSets s = (realColors s, imagColors s)

-- | Swaps foreground and background colors within a 'Coloring'.
invert :: Coloring -> Coloring
invert (Coloring fg bg) = Coloring bg fg

-- | The SGR command corresponding to a particular 'ConsoleLayer' and 'AnsiColor'.
interpAnsiColor :: ConsoleLayer -> AnsiColor -> SGR
interpAnsiColor l (AnsiColor i c) = SetColor l i c

-- | Set the given 'AnsiColor' on the given 'ConsoleLayer'.
setColor :: ConsoleLayer -> AnsiColor -> IO ()
setColor l c = setSGR [interpAnsiColor l c]

-- | Easily create a 'Coloring' by specifying the background 'AnsiColor' and no custom foreground.
fromBG :: AnsiColor -> Coloring
fromBG c = Coloring Nothing (Just c)

-- | Easily create a 'Coloring' by specifying the foreground 'AnsiColor' and no custom background.
fromFG :: AnsiColor -> Coloring
fromFG c = Coloring (Just c) Nothing

-- | Produce a (possibly empty) list of 'SGR' commands from a 'ConsoleLayer' and 'AnsiColor'.
--   An empty 'SGR' list is equivalent to 'Reset'.
sgrList :: ConsoleLayer -> Maybe AnsiColor -> [SGR]
sgrList l = fmap (interpAnsiColor l) . maybe [] (\x -> [x])

-- | Apply both foreground and background color contained in a 'Coloring'.
applyColoring :: Coloring -> IO ()
applyColoring (Coloring fg bg) = do
  setSGR [Reset]
  setSGR $ sgrList Foreground fg ++ sgrList Background bg

-- | Clear any SGR settings and then flush stdout.
clear :: IO ()
clear = setSGR [Reset] *> hFlush stdout

-- | Clear any SGR settings, flush stdout and print a new line.
clearLn :: IO ()
clearLn = clear *> putStrLn ""

-- | Use a particular ANSI 'Coloring' to print a string at the terminal (without a new line),
--   then clear all ANSI SGR codes and flush stdout.
colorStr :: Coloring -> String -> IO ()
colorStr c s = do
  applyColoring c
  putStr s
  clear

-- | Use a particular ANSI 'Coloring' to print a string at the terminal,
--   then clear all ANSI SGR codes, flush stdout and print a new line.
colorStrLn :: Coloring -> String -> IO ()
colorStrLn c s = do
  applyColoring c
  putStr s
  clearLn

-- | Like 'colorStr' but prints bold text.
boldStr :: Coloring -> String -> IO ()
boldStr c s = do
  applyColoring c
  setSGR [SetConsoleIntensity BoldIntensity]
  putStr s
  clear

-- | Like 'colorStrLn' but prints bold text.
boldStrLn :: Coloring -> String -> IO ()
boldStrLn c s = do
  applyColoring c
  setSGR [SetConsoleIntensity BoldIntensity]
  putStr s
  clearLn
