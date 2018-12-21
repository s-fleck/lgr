
<!-- README.md is generated from README.Rmd. Please edit that file -->
yog <img src="man/figures/yog-logo-plain.svg" align="right" width=160 height=160/>
==================================================================================

[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing) [![Travis build status](https://travis-ci.org/s-fleck/yog.svg?branch=master)](https://travis-ci.org/s-fleck/yog) [![Codecov test coverage](https://codecov.io/gh/s-fleck/yog/branch/master/graph/badge.svg)](https://codecov.io/gh/s-fleck/yog?branch=master)

yog is a fully featured logging package for R built on the back of [R6](https://github.com/r-lib/R6) classes. It is designed to be flexible, performant and extensible.

Users that have not worked with R6 classes before, will find the way in which Loggers are configured in yog a bit strange, but I did my best to compose a hopefully helpful . Users that come from python or Java, will feel at home as yog borrows heavily from [Apache Log4j](https://logging.apache.org/log4j/2.x/) and [Python logging](https://docs.python.org/3/library/logging.html).

Usage
-----

To log an *event* with with yog we call `yog$<logging function>()`. Unnamed arguments to the logging function are interpreted by `sprintf()`.

``` r
yog$fatal("A critical error")
#> FATAL [15:03:34.674] A critical error
yog$error("A less severe error")
#> ERROR [15:03:34.701] A less severe error
yog$warn("A potentially bad situation")
#> WARN  [15:03:34.758] A potentially bad situation
yog$info("iris has %s rows", nrow(iris))
#> INFO  [15:03:34.761] iris has 150 rows

# the following log levels are hidden by default
yog$debug("A debug message")
yog$trace("A finer grained debug message")
```

A Logger can have several Appenders. For example, we can add a JSON appender to log to a file with little effort.

``` r
tf <- tempfile()
yog$add_appender(AppenderJson$new(tf))
yog$info("cars has %s rows", nrow(cars))
#> INFO  [15:03:34.777] cars has 50 rows

cat(readLines(tf))
#> {"level":400,"timestamp":"2018-12-21 15:03:34","caller":"eval","msg":"cars has 50 rows"}
```

JSON naturally supports custom fields. Named arguments passed to `info()`, `warn()`, etc... are intepreted as custom fields.

``` r
yog$info("loading cars", "cars", rows = nrow(cars), cols = ncol(cars))
#> INFO  [15:03:34.786] loading cars

cat(readLines(tf), sep = "\n")
#> {"level":400,"timestamp":"2018-12-21 15:03:34","caller":"eval","msg":"cars has 50 rows"}
#> {"level":400,"timestamp":"2018-12-21 15:03:34","caller":"eval","msg":"loading cars","cols":2,"rows":50}
```

For more examples please see the package [vignette](https://s-fleck.github.io/yog/articles/yog.html) and [documentation](https://s-fleck.github.io/yog/)

Features
--------

-   Sensible default configuration for basic usage
-   Hierarchical loggers like in log4j and python logging. This is useful if you want to be able to configure logging on a per-package basis.
-   An arbitrary number of appenders for each logger. A single logger can write values to the console, a logfile, a database, etc... .
-   Vectorized logging (so `yog$fatal(capture.output(iris))` works)
-   Lightning fast in-memory appender based in `data.table` included for interactive use.
-   Support for advanced appenders such as database appenders, a cached appender, etc... .
-   Optional color support via [crayon](https://github.com/r-lib/crayon)
-   Fully featured, but also focused on performance. Benchmarks will follow in due time.

Development Status
------------------

The internal architecture of yog stable and tested. The current development focus is on completing the documentation and irioning out the details for database appenders

Dependencies
------------

[R6](https://github.com/r-lib/R6): The R6 class system prevents the framework on which yog is built and the **only Package yog will ever depend on**.

### Optional Dependencies

These optional dependencies that are not necessary to use yog, but that are required for some extra appenders. Care was taken to choose packages that are slim, stable, have minimal dependencies, and are well mentained:

-   [crayon](https://github.com/r-lib/crayon) for colored console output.
-   [data.table](https://github.com/Rdatatable/) for fast in-memroy logging with `AppenderMemoryDt`.
-   [jsonlite](https://github.com/jeroen/jsonlite) for JSON logging via `LayoutJson`. JSON is a populat plaintext based file format that is easy to read for humans and machines alike.
-   [DBI](https://github.com/r-dbi/DBI) for logging to databases. Logging with yog has been tested with the following backends:
    -   [RSQLite](https://github.com/r-dbi/RSQLite),
    -   [RMariaDB](https://cran.r-project.org/web/packages/RMySQL/index.html) for MariaDB and MySQL,
    -   [RPostgreSQL](https://cran.r-project.org/web/packages/RPostgreSQL/index.html),
    -   [RJDBC](https://github.com/s-u/RJDBC) for DB2.

    In theory all DBI compliant database packages should work. If you are using yog with a database backend, please report your (positive and negative) experiences to me.
-   [whoami](https://github.com/r-lib/whoami/blob/master/DESCRIPTION) for guessing the user name from various sources. You can also set the user name manually if you want to use it for logging.

Installation
------------

``` r
devtools::install.github("s-fleck/yog")
```

Outlook
-------

The long term goal is to support (nearly) all features of the python logging module. If you have experience with python logging or Log4j and are missing features/appenders that you'd like to see, please feel free to post a feature request on the issue tracker.

Acknowledgements
----------------

-   [Inkscape](https://inkscape.org/) for the hex sticker
-   [draw.io](https://draw.io/) for the flow chart in the vignette
