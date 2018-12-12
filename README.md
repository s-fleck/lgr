# yog <img src="man/figures/yog-logo-plain.svg" align="right" width=160 height=160/>

[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Travis build status](https://travis-ci.org/s-fleck/yog.svg?branch=master)](https://travis-ci.org/s-fleck/yog)

Yog is a performant and fully featured logging Package for R that aims to 
provide a more comprehensive and flexible logging solution than existing 
packages. It is built on the back of [R6](https://github.com/r-lib/R6) classes, 
and does not hide that fact. 

Users that have not worked with R6 classes before, 
will find the code examples a bit strange, but I did my best to compose a 
hopefully helpful [package vignette](http://rpubs.com/hoelk/448497). Users
that come from python or Java, will feel quite at home as yog borrows heavily 
from [Apache Log4j](https://logging.apache.org/log4j/2.x/) and 
[Python logging](https://docs.python.org/3/library/logging.html). 


## Features

* Works out of the box for basic usage
* Hierarchical loggers like in log4j and python logging.  
* An arbitrary number of appenders for each logger. A single logger can write
  values to the console, logfile, database, etc... . Each appender has its
  own logging threshold.
* Vectorized logging (so `yog$fatal(capture.output(iris))` works)
* Lightning fast in-memory appender based in `data.table` included for 
  interactive use and (in the future possible) cached appending
* Optional color support via [crayon](https://github.com/r-lib/crayon)
* Designed to be extensible and support a wide variety of appenders. 
* Slim & perfomant (comparisons to to other logging frameworks for R will follow)


## Development Status

The core of Yog is quite stable and tested, but the documentation is still a 
bit lacking. As soon as the documentation is finalized and a few additional
features are implemented to distinquish yog from existing solutions, a stable 
version will be published to CRAN.


## Dependencies

[R6](https://github.com/r-lib/R6): The R6 class system prevents the framework
on which yog is built and the **only Package yog will ever depend on**.

There are also **optional dependencies** that are not necessary to use yog, but
that provide additonal features/appenders. Care was taken to choose packages 
that are slim, stable, have minimal dependencies, and are well mentained:

  * [crayon](https://github.com/r-lib/crayon) for colored console output.
  * [data.table](https://github.com/Rdatatable/) for fast in-memroy logging
    with `AppenderMemoryDt`. 
  * [jsonlite](https://github.com/jeroen/jsonlite) for JSON logging of via 
    `LayoutJson`. 
  * [glue](https://github.com/tidyverse/glue) for `LayoutGlue` (wip)
  * [whoami](https://github.com/r-lib/whoami/blob/master/DESCRIPTION) for 
    guessing the user name from various sources. You can also set the user name 
    manually if you want to use it for logging.

## Installation

``` r
devtools::install.github("s-fleck/yog")
```

## Outlook

The long term goal is to support (nearly) all features of the python logging
module. If you have experience with python logging or Log4j and are missing
features/appenders that you'd like to see, please feel free to post a feature 
request on the issue tracker.



## Acknowledgements

* [Inkscape](https://inkscape.org/) for the hex sticker
* [draw.io](https://draw.io/) for the flow chart in the vignette
