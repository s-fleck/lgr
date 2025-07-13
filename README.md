
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lgr

[![CRAN
status](https://www.r-pkg.org/badges/version/lgr)](https://cran.r-project.org/package=lgr)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://lifecycle.r-lib.org/articles/stages.html)

lgr is a logging package for R built on the back of
[R6](https://github.com/r-lib/R6) classes. It is designed to be
flexible, performant and extensible. The package
[vignette](https://s-fleck.github.io/lgr/articles/lgr.html) contains a
comprehensive description of the features of lgr (some of them unique
among R logging packages) along with many code examples.

Users that have not worked with R6 classes before, will find configuring
Loggers a bit strange and verbose, but care was taken to keep the syntax
for common logging tasks and interactive usage simple and concise. User
that have experience with [shiny](https://github.com/rstudio/shiny),
[plumber](https://github.com/rstudio/plumber), [python
logging](https://docs.python.org/3/library/logging.html) or [Apache
Log4j](https://logging.apache.org/log4j/2.x/) will feel at home. User
that are proficient with R6 classes will also find it easy to extend and
customize lgr, for example with their own appenders Loggers or
Appenders.

## Features

- *Hierarchical loggers* like in log4j and python logging. This is
  useful if you want to be able to configure logging on a per-package
  basis.
- An *arbitrary number of appenders* for each logger. A single logger
  can write to the console, a logfile, a database, etc… .
- Support for structured logging. As opposed to many other logging
  packages for R a log event is not just a message with a timestamp, but
  an object that can contain arbitrary data fields. This is useful for
  producing machine readable logs.
- *Vectorized* logging (so `lgr$fatal(capture.output(iris))` works)
- Lightning fast *in-memory logs* for interactive use.
- Appenders that write logs to a wide range of destinations:
  - databases (buffered or directly)
  - email or pushbullet
  - plaintext files (with a powerful formatting syntax)
  - JSON files with arbitrary data fields
  - Rotating files that are reset and backed-up after they reach a
    certain file size or age
  - memory buffers
  - (colored) console output
- Optional support to use [glue](https://glue.tidyverse.org/) instead of
  `sprintf()` for composing log messages.

## Usage

To log an *event* with with lgr we call `lgr$<logging function>()`.
Unnamed arguments to the logging function are interpreted by
`sprintf()`. For a way to create loggers that
[glue](https://glue.tidyverse.org/) instead please refer to the
vignette.

``` r
lgr$fatal("A critical error")
#> FATAL [11:32:48.843] A critical error
lgr$error("A less severe error")
#> ERROR [11:32:48.864] A less severe error
lgr$warn("A potentially bad situation")
#> WARN  [11:32:48.868] A potentially bad situation
lgr$info("iris has %s rows", nrow(iris))
#> INFO  [11:32:48.869] iris has 150 rows

# the following log levels are hidden by default
lgr$debug("A debug message")
lgr$trace("A finer grained debug message")
```

A Logger can have several Appenders. For example, we can add a JSON
appender to log to a file with little effort.

``` r
tf <- tempfile()
lgr$add_appender(AppenderFile$new(tf, layout = LayoutJson$new()))
lgr$info("cars has %s rows", nrow(cars))
#> INFO  [11:32:48.878] cars has 50 rows
cat(readLines(tf))
#> {"level":400,"timestamp":"2025-07-13 11:32:48","logger":"root","caller":"eval","msg":"cars has 50 rows","rawMsg":"cars has %s rows"}
```

By passing a named argument to the log function, you can log not only
text but arbitrary R objects. Not all appenders support structured
logging perfectly, but JSON does. This way you can create logfiles that
are machine as well as (somewhat) human readable.

``` r
lgr$info("loading %s", "cars", rows = nrow(cars), cols = ncol(cars), vector = c(1, 2, 3))
#> INFO  [11:32:48.893] loading cars {rows: `50`, cols: `2`, vector: (1, 2, 3)}
cat(readLines(tf), sep = "\n")
#> {"level":400,"timestamp":"2025-07-13 11:32:48","logger":"root","caller":"eval","msg":"cars has 50 rows","rawMsg":"cars has %s rows"}
#> {"level":400,"timestamp":"2025-07-13 11:32:48","logger":"root","caller":"eval","msg":"loading cars","rawMsg":"loading %s","rows":50,"cols":2,"vector":[1,2,3]}
```

For more examples please see the package
[vignette](https://s-fleck.github.io/lgr/articles/lgr.html) and
[documentation](https://s-fleck.github.io/lgr/)

## See lgr in action

lgr is used to govern console output in my shiny based csv editor
[shed](https://github.com/s-fleck/shed)

``` r
# install.packages("remotes")
remotes::install_github("s-fleck/shed")
library(shed)

# log only output from the "shed" logger to a file
logfile <- tempfile()
lgr::get_logger("shed")$add_appender(AppenderFile$new(logfile))
lgr::threshold("all")

# edit away and watch the rstudio console!
lgr$info("starting shed")
shed(iris)  
lgr$info("this will not end up in the log file")

readLines(logfile)

# cleanup
file.remove(logfile)
```

## Development status

lgr is stable and safe for use. I’ve been using it in production code
for several years myself. There has been very little recent development
because it’s pretty stable and contains (nearly) all planned features.

Notable points that are still planned (without specific ETA):

- Support for config files is heavily experimental and incomplete. This
  is an important basic feature, but I have not yet found a great way to
  support this in a generic way. For now, I recommend you come up with
  your own solution if you need to lgr to work in a production
  environment that relies on config files.

- Improve the documentation. The documentation should be mostly
  complete, but is not perfect. If there’s something missing or
  something you don’t understand, please ask (for example via a github
  issue).

## Dependencies

[R6](https://github.com/r-lib/R6): The R6 class system provides the
framework on which lgr is built and the **only Package lgr will ever
depend on**. If you are a **package developer** and want to add logging
to your package, this is the only transitive dependency you have to
worry about, as configuring of the loggers should be left to the user of
your package.

### Optional dependencies

lgr comes with a long list of optional dependencies that make a wide
range of appenders possible. You only need the dependencies for the
Appenders you actually want to use. Care was taken to choose packages
that are slim, stable, have minimal dependencies, and are well
maintained :

Extra appenders (in the main package):

- [jsonlite](https://github.com/jeroen/jsonlite) for JSON logging via
  `LayoutJson`. JSON is a popular plaintext based file format that is
  easy to read for humans and machines alike.

- [rotor](https://github.com/s-fleck/rotor) for log rotation via
  AppenderFileRotating and co.

- [data.table](https://github.com/Rdatatable/) for fast in-memory
  logging with `AppenderDt`, and also by all database / DBI Appenders.

- [glue](https://glue.tidyverse.org/) for a more flexible formatting
  syntax via LoggerGlue and LayoutGlue.

Extra appenders via lgrExtra:

- For support for Elasticsearch, Dynatrace, Push- and Email
  notifications, etc… as well as the relevant dependencies please refer
  to the documentation of
  [lgrExtra](https://github.com/s-fleck/lgrExtra)

Other extra features:

- [yaml](https://CRAN.R-project.org/package=yaml) for configuring
  loggers via YAML files (experimental)
- [crayon](https://github.com/r-lib/crayon) for colored console
  output.  
- [whoami](https://github.com/r-lib/whoami/blob/master/DESCRIPTION) for
  guessing the user name from various sources. You can also set the user
  name manually if you want to use it for logging.
- [desc](https://CRAN.R-project.org/package=desc) for the package
  development convenience function `use_logger()`
- [cli](https://CRAN.R-project.org/package=cli) for printing the tree
  structure of registered loggers with `logger_tree()`

Other `Suggests` ([future](https://CRAN.R-project.org/package=future),
[future.apply](https://CRAN.R-project.org/package=future.apply)) do not
provide extra functionality but had to be included for some of the
automated unit tests run by lgr.

## Installation

You can install lgr from CRAN

``` r
install.packages("lgr")
```

Or you can install the current development version directly from github

``` r
#install.packages("remotes")
remotes::install_github("s-fleck/lgr")
```

## Outlook

The long term goal is to support (nearly) all features of the python
logging module. If you have experience with python logging or Log4j and
are missing features/appenders that you’d like to see, please feel free
to post a feature request on the issue tracker.

## Acknowledgement

- [diagrams.net](https://app.diagrams.net/) for the flow chart in the
  vignette
