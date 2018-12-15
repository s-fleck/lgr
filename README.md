# yog <img src="man/figures/yog-logo-plain.svg" align="right" width=160 height=160/>

[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Travis build status](https://travis-ci.org/s-fleck/yog.svg?branch=master)](https://travis-ci.org/s-fleck/yog)
[![Codecov test coverage](https://codecov.io/gh/s-fleck/yog/branch/master/graph/badge.svg)](https://codecov.io/gh/s-fleck/yog?branch=master)

Yog is a fully featured logging package for R built on the back 
of [R6](https://github.com/r-lib/R6) classes. It is designed to be flexible,
performant and extensible. 

Users that have not worked with R6 classes before, will find the way in which
Loggers are configured in Yog a bit strange, but I did my best to compose a 
hopefully helpful [package vignette](http://rpubs.com/hoelk/448497). 
Users that come from python or Java, will feel at home as yog borrows heavily 
from [Apache Log4j](https://logging.apache.org/log4j/2.x/) and
[Python logging](https://docs.python.org/3/library/logging.html). 


## Features

* Works out of the box for basic usage
* Hierarchical loggers like in log4j and python logging. This is useful if you
  want to be able to configure logging on a per-package basis.
* An arbitrary number of appenders for each logger. A single logger can write
  values to the console, a logfile, a database, etc... .
* Vectorized logging (so `yog$fatal(capture.output(iris))` works)
* Lightning fast in-memory appender based in `data.table` included for 
  interactive use.
* Support for advanced appenders such as database appenders, a cached appender,
  etc... .
* Optional color support via [crayon](https://github.com/r-lib/crayon)
* Fully featured, but also focused on performance. Benchmarks will follow in
  due time.


## Development Status

The internal architecture of Yog stable and tested. The current development 
focus is on completing the documentation, and adding new appenders so that
yog has user-visible advantages over existing logging frameworks for R.
A first CRAN release is planned for roughly April 2019.


## Dependencies

[R6](https://github.com/r-lib/R6): The R6 class system prevents the framework
on which yog is built and the **only Package yog will ever depend on**.

There are also **optional dependencies** that are not necessary to use yog, but
that are required for some more advanced appenders. Care was taken to choose 
packages that are slim, stable, have minimal dependencies, and are well 
mentained:

  * [crayon](https://github.com/r-lib/crayon) for colored console output.
  * [data.table](https://github.com/Rdatatable/) for fast in-memroy logging
    with `AppenderMemoryDt`. 
  * [jsonlite](https://github.com/jeroen/jsonlite) for JSON logging of via 
    `LayoutJson`. 
  * [DBI](https://github.com/r-dbi/DBI), 
    [RSQLite](https://github.com/r-dbi/RSQLite), 
    [RJDBC](https://github.com/s-u/RJDBC) for logging to databases. In theorey
    all DBI complient database packages for R should be supported. If you
    are using Yog with a database backend, please report your (positive and
    negative) experiences to me.
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
