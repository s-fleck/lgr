# yog <img src="man/figures/yog-logo-plain.svg" align="right" width=160 height=160/>

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build status](https://travis-ci.org/s-fleck/yog.svg?branch=master)](https://travis-ci.org/s-fleck/yog)

yog is yet another logging package for R. It is built on the back of
[R6](https://github.com/r-lib/R6) and heavily inspired by 
[Apache Log4j](https://logging.apache.org/log4j/2.x/) and 
[Python logging](https://docs.python.org/3/library/logging.html).

## Features
  
* An arbitrary number of appenders for each logger. A single logger can write
  values to the console, logfile, database, etc... . Each appender has its
  own logging threshold.
* Hierarchical loggers like in log4j and python logging.
* Vectorized logging (so `yog$fatal(capture.output(iris))` works)
* Lightning fast in-memory appender based in `data.table` included for 
  interactive use and (in the future possible) cached appending
* Optional color support via colt and crayon


## Dependencies

[R6](https://github.com/r-lib/R6): The R6 class system prevents the framework
on which yog is built and the **only Package yog will ever depend on**.

There are also **optional dependencies** that are not necessary to use yog, but
that provide additonal features. Care was taken to choose packages that are
slim, stable, have minimal dependencies, and are well mentained:

  * [crayon](https://github.com/r-lib/crayon) and 
    [colt](https://github.com/s-fleck/colt) for color output. Colt is just a 
    small wrapper around crayon that I wrote myself to support color themes 
    in the console.
  * [data.table](https://github.com/Rdatatable/) for `AppenderMemoryDt`, 
    for fast in-memory logging. 
  * [jsonlite](https://github.com/jeroen/jsonlite) for JSON logging of via 
    `LayoutJson()`. 
  * [glue](https://github.com/tidyverse/glue) for `LayoutGlue`
  * [whoami](https://github.com/r-lib/whoami/blob/master/DESCRIPTION) for 
    guessing the user name from various sources. You can also set the user name 
    manually if you want to use it for logging.

## Installation

``` r
devtools::install.github("s-fleck/yog")
```

## Outlook

The long term goal is to support (nearly) all features of the python logging
module. Feel free to add a feature request to the issue tracker if you require
something special.




## Acknowledgements

* [Inkscape](https://inkscape.org/) for the hex sticker
* [draw.io](https://draw.io/) for the flow chart in the vignette
