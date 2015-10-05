# ansigraph

[![Build Status](https://travis-ci.org/BlackBrane/ansigraph.svg?branch=master)](https://travis-ci.org/BlackBrane/ansigraph)
[![Hackage](https://img.shields.io/hackage/v/ansigraph.svg)]()

__Terminal-based graphing via ANSI and Unicode__

```
▁▂▃▄▄▅▆▇▇█████▇▇▆▆▅▄▃▃▂▂▁▁▁▁▂▂▃▃▄▅▆▆▇▇█████▇▇▆▅▄▄▃▂▁
```
### Overview

Ansigraph is an ultralightweight terminal-based graphing utility. It uses unicode characters and ANSI escape codes to display and animate colored graphs of vectors/functions in real and complex variables.

This functionality is provided by a `Graphable` type class, whose method `graphWith` draws a graph at the terminal. Another function `animateWith` takes a list of Graphable elements and displays an animation by rendering them in sequence. Both of these functions take an options record as an argument. The `graph` and `animate` functions are defined to use the default options, and the user can define similar functions based on their own settings.

### Usage

There are two main ways to use the package.

Importing `System.Console.Ansigraph` provides all the functionality we typically use. This includes the _FlexibleInstances_ extension, which makes it marginally more convenient to use graphing functions by allowing instances like `Graphable [Double]`.

If you want to use the package without activating _FlexibleInstances_ then you can import `System.Console.Ansigraph.Core`, which provides everything except these instances. To graph you must use one of a handful of newtype wrappers, namely: `Graph`, `PosGraph`, `CGraph`, `Mat`, `CMat`. These wrappers are also available from the standard `Ansigraph` module.

Some of these wrappers may be useful even when working from the main module because it indicates a particular kind of graph is to be used. For example, `PosGraph` holds the same type as `Graph`, but it uses a graph style that assumes its data to be positive. For the same reason it may prove useful to define other wrapper data types when new approaches to terminal graphing are devised.

### Examples

```
λ> import System.Console.Ansigraph
λ> let v = [1..30] :: [Double]
λ> graph v
▁▁▁▁▂▂▂▂▃▃▃▃▄▄▄▅▅▅▅▆▆▆▆▇▇▇▇██

```
