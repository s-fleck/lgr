
<!-- README.md is generated from README.Rmd. Please edit that file -->
lgr
===

[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing) [![Travis build status](https://travis-ci.org/s-fleck/lgr.svg?branch=master)](https://travis-ci.org/s-fleck/lgr) [![Codecov test coverage](https://codecov.io/gh/s-fleck/lgr/branch/master/graph/badge.svg)](https://codecov.io/gh/s-fleck/lgr?branch=master)

lgr is a fully featured logging package for R built on the back of [R6](https://github.com/r-lib/R6) classes. It is designed to be flexible, performant and extensible.

Users that have not worked with R6 classes before, will find the way in which Loggers are configured in lgr a bit strange, but I did my best to compose a hopefully helpful . Users that come from python or Java, will feel at home as lgr borrows heavily from [Apache Log4j](https://logging.apache.org/log4j/2.x/) and [Python logging](https://docs.python.org/3/library/logging.html).

Usage
-----

To log an *event* with with lgr we call `lgr$<logging function>()`. Unnamed arguments to the logging function are interpreted by `sprintf()`.

``` r
lgr$fatal("A critical error")
#> FATAL [09:03:34.452] A critical error
lgr$error("A less severe error")
#> ERROR [09:03:34.478] A less severe error
lgr$warn("A potentially bad situation")
#> WARN  [09:03:34.503] A potentially bad situation
lgr$info("iris has %s rows", nrow(iris))
#> INFO  [09:03:34.505] iris has 150 rows

# the following log levels are hidden by default
lgr$debug("A debug message")
lgr$trace("A finer grained debug message")
```

A Logger can have several Appenders. For example, we can add a JSON appender to log to a file with little effort.

``` r
tf <- tempfile()
lgr$add_appender(AppenderJson$new(tf))
lgr$info("cars has %s rows", nrow(cars))
#> INFO  [09:03:34.527] cars has 50 rows

cat(readLines(tf))
#> {"level":400,"timestamp":"2018-12-28 09:03:34","caller":"eval","msg":"cars has 50 rows"}
```

JSON naturally supports custom fields. Named arguments passed to `info()`, `warn()`, etc... are intepreted as custom fields.

``` r
lgr$info("loading cars", "cars", rows = nrow(cars), cols = ncol(cars))
#> INFO  [09:03:34.536] loading cars {cols: 2, rows: 50}

cat(readLines(tf), sep = "\n")
#> {"level":400,"timestamp":"2018-12-28 09:03:34","caller":"eval","msg":"cars has 50 rows"}
#> {"level":400,"timestamp":"2018-12-28 09:03:34","caller":"eval","msg":"loading cars","cols":2,"rows":50}
```

For more examples please see the package [vignette](https://s-fleck.github.io/lgr/articles/lgr.html) and [documentation](https://s-fleck.github.io/lgr/)

Features
--------

-   Sensible default configuration for basic usage
-   Hierarchical loggers like in log4j and python logging. This is useful if you want to be able to configure logging on a per-package basis.
-   An arbitrary number of appenders for each logger. A single logger can write values to the console, a logfile, a database, etc... .
-   Vectorized logging (so `lgr$fatal(capture.output(iris))` works)
-   Lightning fast in-memory appender based in `data.table` included for interactive use.
-   Support for advanced appenders such as database appenders, a cached appender, etc... .
-   Optional color support via [crayon](https://github.com/r-lib/crayon)
-   Fully featured, but also focused on performance. Benchmarks will follow in due time.

Development Status
------------------

The core of lgr is more or less complete and usable. What is currently lacking is the documentation. I also want to conduct a proper field-test before I release it on CRAN, so don't expect a CRAN release before Februar/March 2019.

Dependencies
------------

[R6](https://github.com/r-lib/R6): The R6 class system prevents the framework on which lgr is built and the **only Package lgr will ever depend on**.

### Optional Dependencies

These optional dependencies that are not necessary to use lgr, but that are required for some extra appenders. Care was taken to choose packages that are slim, stable, have minimal dependencies, and are well mentained:

-   [crayon](https://github.com/r-lib/crayon) for colored console output.
-   [data.table](https://github.com/Rdatatable/) for fast in-memroy logging with `AppenderDt`.
-   [jsonlite](https://github.com/jeroen/jsonlite) for JSON logging via `LayoutJson`. JSON is a populat plaintext based file format that is easy to read for humans and machines alike.
-   [DBI](https://github.com/r-dbi/DBI) for logging to databases. Logging with lgr has been tested with the following backends:
    -   [RSQLite](https://github.com/r-dbi/RSQLite),
    -   [RMariaDB](https://cran.r-project.org/web/packages/RMySQL/index.html) for MariaDB and MySQL,
    -   [RPostgreSQL](https://cran.r-project.org/web/packages/RPostgreSQL/index.html),
    -   [RJDBC](https://github.com/s-u/RJDBC) for DB2.

    In theory all DBI compliant database packages should work. If you are using lgr with a database backend, please report your (positive and negative) experiences to me.
-   [whoami](https://github.com/r-lib/whoami/blob/master/DESCRIPTION) for guessing the user name from various sources. You can also set the user name manually if you want to use it for logging.

Installation
------------

``` r
devtools::install.github("s-fleck/lgr")
```

Outlook
-------

The long term goal is to support (nearly) all features of the python logging module. If you have experience with python logging or Log4j and are missing features/appenders that you'd like to see, please feel free to post a feature request on the issue tracker.

Acknowledgements
----------------

-   [Inkscape](https://inkscape.org/) for the hex sticker
-   [draw.io](https://draw.io/) for the flow chart in the vignette
