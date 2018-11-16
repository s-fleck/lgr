# Yog

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

yog is yet another logging package for R that is build on the back of
R6.

## Features
  
* An arbitrary number of appenders for each logger. A single logger can write
  values to the console, logfile, database, etc... . Each appender has its
  own logging threshold.
* Vectorized logging (so `yog$fatal(capture.output(iris))` works)
* Lightning fast in-memory appender based in `data.table` included for 
  interactive use and (in the future possible) cached appending
* Optional color support via colt and crayon


## Dependencies

  * `R6`: The R6 class system prevents the framework on which yog is built. This
  is the only Package yog will ever depend on.

There are also some suggested packages that are not necessary to use Yog, but
that provide additonal features. Care was taken to choose packages that are
slim themselves and only depend on base packages:

  * `crayon` and `colt` for color output
  * `data.table` for `AppenderMemoryDt`, for fast in-memory logging
  * `jsonlite` for JSON logging of via `LayoutJson()`. 
  * `glue` for `LayoutGlue`


## Installation

``` r
devtools::install.github("s-fleck/yog")
```
