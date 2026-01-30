# lgr: A fully featured logging framework for R

## Introduction

lgr is a logging framework for R inspired by [Apache
Log4j](https://logging.apache.org/log4j/2.x/) and [Python
logging](https://docs.python.org/3/library/logging.html). It follows an
object oriented design implemented through [R6
classes](https://github.com/r-lib/R6). This enables lgr to have a larger
set of features than other logging packages for R, and makes it flexible
and easy to extend.

If you are not sure if lgr is the right package for you, take a look
[examples](#examples) to see what lgr can do for you.

### Quickstart

lgr comes with a so-called **root logger** that is ready to go after you
install it. If you are a package developer you can (and should) set up a
new Logger for your package, but more on that later. For many use cases
the root Logger will suffice.

#### Logging to the console

``` r
# the root logger is called "lgr"
lgr$info("Vampire stories are generally located in Styria.")
```

You can use formatting strings that are passed on to
[`sprintf()`](https://rdrr.io/r/base/sprintf.html) in lgr.

``` r
lgr$error("Vampires generally arrive in carriages drawn by %i black horses.", 2)
```

#### Logging to plaintext files

Usually you don’t (only) want to log to the console, you want to log to
files. Output of Loggers is managed by Appenders. The root Logger is
preconfigured with a console Appender (that is why we see output in the
console). Let’s add a file Appender:

``` r
tf <- tempfile(fileext = ".info")
lgr$add_appender(AppenderFile$new(tf), name = "file")
lgr$info("You must think I am joking")
readLines(tf)
#> [1] "INFO  [2026-01-30 13:44:05.709] You must think I am joking"
```

The various Appenders available in lgr are R6 classes. To instantiate an
object of that class (i.e. create a new appender) you have to use the
`$new()` function as in the example above. Whenever you see something in
lgr that `IsNamedLikeThis`, you can be sure that it is such an R6 class.

If you look at the output, you see that the timestamp format of the file
appender is slightly different to the timestamp format of the console
Appender. Formatting is handled by **Layouts**, and each Appender has
exactly one:

``` r
lgr$appenders$file$set_layout(LayoutFormat$new(timestamp_fmt = "%B %d %T"))
lgr$info("No, I am quite serious")
readLines(tf)
#> [1] "INFO  [2026-01-30 13:44:05.709] You must think I am joking"
#> [2] "INFO  [January 30 13:44:05] No, I am quite serious"

#cleanup
unlink(tf)
```

#### Logging to JSON files

If you log to files, you should not log normal text. If you want to
analyse your logs later, it’s much better to log to a format like JSON:

``` r
# cleanup behind the old Appender
unlink(tf)  
lgr$remove_appender("file")

# setup a JSON appender
lgr$add_appender(AppenderJson$new(tf), name = "json")
lgr$info("We lived in Styria")
```

JSON is still somewhat human readable

``` r
cat(readLines(tf))
#> {"level":400,"timestamp":"2026-01-30 13:44:05","logger":"root","caller":"eval","msg":"We lived in Styria"}
```

and easy for machines to parse

``` r
read_json_lines(tf)
#>   level           timestamp logger caller                msg
#> 1   400 2026-01-30 13:44:05   root   eval We lived in Styria
```

Many Appenders provide either a `$show()` method and a `$data` active
binding convenience, and so you do not have to call
[`readLines()`](https://rdrr.io/r/base/readLines.html) & co manually.

``` r
# show is a method and takes some extra arguments, like maximum number of lines
# to show
lgr$appenders$json$show()
#> {"level":400,"timestamp":"2026-01-30 13:44:05","logger":"root","caller":"eval","msg":"We lived in Styria"}

# $data always returns a data.frame if available. It is an active binding 
# rather than a method, so no extra arguments are possible
lgr$appenders$json$data  
#>   level           timestamp logger caller                msg
#> 1   400 2026-01-30 13:44:05   root   eval We lived in Styria
```

Please note that under the hood, `AppenderJson` is just an
`AppenderFile` with `LayoutJson`. The only difference is AppenderJson
provides a `$data()` method while AppenderFile does not.

#### Structured Logging (custom fields)

lgr treats a *LogEvent* as a unit of data, not just a message with a
timestamp. A log event can contain arbitrary data, though not all
Appenders can handle that well. The JSON appender we added above is
particularly good at handling most R objects.

``` r
# The default console appender displays custom fields as pseudo-json after the message
lgr$info("Styria has", poultry = c("capons", "turkeys"))

# JSON can store most R objects quite naturally 
read_json_lines(tf)
#>   level           timestamp logger caller                msg         poultry
#> 1   400 2026-01-30 13:44:05   root   eval We lived in Styria            NULL
#> 2   400 2026-01-30 13:44:06   root   eval         Styria has capons, turkeys
read_json_lines(tf)$poultry[[2]]  # works because poultry is a list column
#> [1] "capons"  "turkeys"
```

#### What Else

If the examples above have piqued your interest, the rest of this
vignette will provide more details on the workings of lgr. Discussing
all Appenders and configuration options is beyond the scope of this
vignette, please refer to the [function
reference](https://s-fleck.github.io/lgr/reference/index.html) for that.

## Usage

### Structure of the logging system

If you want custom logging configurations, you have to understand the
structure of the logging process.

- A **Logger** collects information and dispatches it to its
  *Appenders*, and also the *Appenders* of its *Parent Loggers* (also
  see the section on hierarchical logging)
- An **Appender** writes the log message to destination (the console, a
  file, a database, etc…).
- A **Layout** is used by an Appender to format LogEvents. For example,
  `AppenderFile` uses `LayoutFormat` by default to write human readable
  log events to a text file, but can also use `LayoutJson` produce
  machine readable JSON lines logfiles.
- **LogEvents** are produced by the Logger and dispatched to Appenders.
  They contain all the information that is being logged (think of it as
  a row in table). LogEvents usually contain the log level, a timestamp,
  a message, the name of the calling function, and a
  [reference](https://adv-r.hadley.nz/r6.html#r6-semantics%5D) to the
  Logger that created it. In addition, a LogEvent can contain any number
  of custom fields. See [examples 1 & 2](#examples)

#### On R6 classes

The elements described above are R6 classes. R6 is an object orientation
system for R that is used by many popular packages such as shiny, dplyr,
plumber, roxygen2, and testthat but often behind the scenes and not as
exposed as in lgr.

You recognize R6 classes in this package because they are named
following the `UpperCamelCase` naming convention. While there is only
one kind of Logger and one kind of LogEvent, there are several
subclasses of Appenders and Layouts.

An introduction to R6 classes is beyond the scope of this document, but
you can find the official documentation [here](https://r6.r-lib.org/)
and there is also this [talk on
Youtube](https://www.youtube.com/watch?v=3GEFd8rZQgY). In short R6
classes store data (fields) together with functions (methods) and have
to be instantiated with `<classname>$new()`. So if you want to create a
new `AppenderFile`, you do this by calling
`AppenderFile$new(file = tempfile())`.

Please not that Loggers should never be instantiated directly with
`$new()` but always with
[`get_logger()`](https://s-fleck.github.io/lgr/reference/get_logger.md).

### Log levels

lgr supports the standard log4j Log Levels outlined bellow. The Log
Level of an event represents its severity. The named log levels are
really just nicknames for integer values, and you can use the
`character` or `integer` representations interchangeably. You can also
use arbitrary integer values (greater than `0`), but you are encouraged
to stick to the ones bellow.

| Level | Name  | Description                                                                                                                                                                                      |
|------:|:------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|     0 | off   | Tells a Logger or Appender to suspend all logging                                                                                                                                                |
|   100 | fatal | Critical error that leads to program abort. Should always indicate a [`stop()`](https://rdrr.io/r/base/stop.html) or similar                                                                     |
|   200 | error | A severe error that does not trigger program abort                                                                                                                                               |
|   300 | warn  | A potentially harmful situation, like [`warning()`](https://rdrr.io/r/base/warning.html)                                                                                                         |
|   400 | info  | An informational message on the progress of the application                                                                                                                                      |
|   500 | debug | Finer grained informational messages that are mostly useful for debugging                                                                                                                        |
|   600 | trace | An even finer grained message than debug ([more info](https://softwareengineering.stackexchange.com/questions/279690/why-does-the-trace-level-exist-and-when-should-i-use-it-rather-than-debug)) |
|    NA | all   | Tells a Logger or Appender to process all log events                                                                                                                                             |

`off` and `all` are valid thresholds for Appenders and Loggers, but not
valid levels for LogEvents; e.g. `lgr$set_threshold(NA)` makes sense,
but `lgr$log("all", "an example message")` does not.

The list of named log levels is stored as a global option
(`getOption("lgr.log_levels")`) and you can use
[`add_log_levels()`](https://s-fleck.github.io/lgr/reference/get_log_levels.md)
and
[`remove_log_levels()`](https://s-fleck.github.io/lgr/reference/get_log_levels.md)
to define your own named levels if you want to. There are only
predefined logging methods (`lgr$fatal()`, etc..) for the standard log
levels and you have to use `lgr$log(level, message)` to create a
LogEvent with a custom log level.

### Logging with the Root Logger

lgr comes with a pre-configured root Logger. It is called *root* because
you can [set up a tree of Loggers](#hierarchy) that descent from it, but
for basic use you will not have to worry about that.

#### Logging syntax

lgr Loggers are R6 objects with *methods* (functions) for logging. You
can refer to the *root* logger with `lgr`.

``` r
lgr$fatal("This is an important message about %s going wrong", "->something<-")
lgr$trace("Trace messages are still hidden")
lgr$set_threshold("trace")
lgr$trace("Unless we lower the threshold")
```

#### Formatting strings

You can use [`sprintf()`](https://rdrr.io/r/base/sprintf.html) style
formatting strings directly in log messages.

``` r
lgr$info("The sky was the color of %s, tuned to a dead chanel", "television")
```

### LogEvents: The atomic unit of logging

LogEvents are objects that store all information collected by the
Logger. They are passed on to Appenders that output them, but Appenders
usually don’t utilize all the information present in a log event. The
last event produced by a Logger is stored in its `last_event` field.

``` r
lgr$info("Vampire stories are generally located in Styria")
lgr$last_event  # a summary output of the event
#> INFO  [2026-01-30 13:44:06] Vampire stories are generally located in Styria
lgr$last_event$values  # all values stored in the event as a list
#> $level
#> [1] 400
#> 
#> $timestamp
#> [1] "2026-01-30 13:44:06 UTC"
#> 
#> $logger
#> [1] "root"
#> 
#> $caller
#> [1] "eval"
#> 
#> $msg
#> [1] "Vampire stories are generally located in Styria"
#> 
#> $rawMsg
#> [1] "Vampire stories are generally located in Styria"
```

LogEvents can contain not only these standard values, but an arbitrary
number of extra values. These extra values are passed as named arguments
to the logging function (as opposed as to parameters to
[`sprintf()`](https://rdrr.io/r/base/sprintf.html), which are passed as
unnamed arguments). It is up to the Appender whether to process them
further or not.

You should consider making use of structured logging liberally and using
output formats that support them (such as JSON), rather than producing
elaborately formatted but hard to parse log messages.

``` r
# bad
lgr$info("Processing track '%s' with %s waypoints", "track.gpx", 32)

# Good
tf <- tempfile()
lgr$add_appender(AppenderJson$new(tf), "json")
lgr$info("Processing track", file = "track.gpx", waypoints = 32)
lgr$appenders$json$data
#>   level           timestamp logger caller              msg      file waypoints
#> 1   400 2026-01-30 13:44:06   root   eval Processing track track.gpx        32
```

### Thresholds & Filters: controlling output detail

To control the level of detail of the log output, you can set
**thresholds** for Loggers and Appenders. A Logger with a threshold of
`warn` will only create LogEvents of the priorities `warn`, `error` and
`fatal` and dispatch them to its Appenders.

> A **threshold** of a Logger or Appender is the minimum **log level** a
> LogEvent must have so that that Logger/Appender processes it.

If you require more complex logic to decide whether a LogEvent should be
created/processed you can also assign **filters** to Loggers/Appenders.
Filters are just functions that have exactly one argument, `event` (the
LogEvent to be filtered), and return `TRUE` or `FALSE`. They will be
applied after the threshold is checked. Alternatively there is also a
formal R6 class for Filters (?EventFilter) that you can use, but this is
usually not necessary.

examples:

``` r
f1 <- function(event) { grepl("bird", event$msg) }
lgr$set_filters(list(f1))

lgr$info("is it a plane?")
lgr$info("no! is it a bird?")

# since this is not a very useful filter, we better remove it again
lgr$set_filters(NULL)
```

### Appenders: Managing log destinations

The root logger only logs to the console by default. If you want to
redirect the output to a file you can just add a file appender to lgr.

``` r
tf <- tempfile()

# Add a new appender to a logger. We don't have to supply a name, but that
# makes it easier to remove later.
lgr$add_appender(AppenderFile$new(file = tf), name = "file")

# configure lgr so that it logs everything to the file, but only info and above
# to the console
lgr$set_threshold(NA)
lgr$appenders$console$set_threshold("info")
lgr$appenders$file$set_threshold(NA)
lgr$info("Another informational message")
lgr$debug("A debug message not shown by the console appender")

readLines(tf)
#> [1] "INFO  [2026-01-30 13:44:06.784] Another informational message"                    
#> [2] "DEBUG [2026-01-30 13:44:06.786] A debug message not shown by the console appender"

# Remove the appender again
lgr$remove_appender("file")
unlink(tf)
```

### Inheritance: Hierarchical Loggers

Logger hierarchies are a powerful concept to organize logging for
different parts of a larger system. This is mainly relevant for package
developers. It is good practice to have a separate Logger for each
package. Since it is not common in R to build complex systems of
hierarchically organised packages, hierarchies will usually be pretty
flat (i.e. most Loggers will only inherit from the root logger).

Each newly created Logger is child to a parent Logger, derived from its
name. So `lg <- get_logger("foo/bar")` creates the logger with the
qualified name `foo/bar` whose parent is the logger `foo` whose parent
is (implicitly) the `root` logger. If the logger `foo` does not exist in
that scenario, it is created automatically. This behaviour might sound
strange at first, but it mimics tried and tested behaviour of python
logging. This way logging is decoupled from the business logic and your
program will not abort if you forgot to initialize some logger up the
hierarchy for whatever reason.

A logger dispatches the LogEvents it creates not only to its own
Appenders, but also to the Appenders of all its ancestral Loggers
(ignoring the threshold and Filters of the ancestral Loggers, but not of
the Appenders). When you define Loggers for your package, you should
*not* configure them (with custom Appenders or thresholds); that should
be left to the user of the package.

If all this sounds confusing to you, take a look at the
[examples](#examples-hierarchy) and
[`?logger_tree`](https://s-fleck.github.io/lgr/reference/logger_tree.md).
The common use cases are pretty easy to understand and illustrate the
*how* and *why* pretty well.

Example hierarchy for the package **fancymodel** that provides a model
along with a plumber API and a shiny web-interface to the package.

``` r
# prints a tree structure of all registered loggers
logger_tree()
```

    root [info] -> 1 appenders
    └─fancymodel
      ├─plumber
      └─#shiny
        ├─server [trace] -> 2 appenders
        └─ui -> 1 appenders

#### Log flow

The graph bellow outlines the flow of LogEvents through the logging
system. This is an important reference if you want to work with Filters
and Logger hierarchies.

![](log_flow.svg)

### Logging with LoggerGlue

[glue](https://glue.tidyverse.org/) is very nicely designed package for
string interpolation. It makes composing log messages more flexible and
comfortable at the price of an additional dependency and slightly less
performance than [`sprintf()`](https://rdrr.io/r/base/sprintf.html)
(which is used by normal Loggers). To take advantage of glue, simply
create a new LoggerGlue like this:

``` r
# install.packages("glue")

lg <- get_logger_glue("glue/logger")

lg$info(
  "glue automatically ", 
  "pastes together unnamed arguments ",
  "and evaluates arbitray expressions inside braces {Sys.Date()}"
)
```

Glue lets you define temporary variables inside the
[`glue()`](https://glue.tidyverse.org/reference/glue.html) call. As with
the normal Logger, named arguments get turned into custom fields.

``` r
lg$info("For more info on glue see {website}", website = "https://glue.tidyverse.org/")
```

You can suppress this behaviour by making named argument start with a
`"."`.

``` r
lg$info("Glue is available from {.cran}", .cran = "https://CRAN.R-project.org/package=glue")
```

## Configuration

### With setters

There are several different ways to configure loggers. The most straight
forward one is to use *setters* to specify the Loggers properties.

``` r
lg <- get_logger("test")
lg$config(NULL)  # resets logger to unconfigured state
#> <Logger> [all] test
#> 
#> inherited appenders:
#>   console: <AppenderConsole> [info] -> console
lg$set_threshold("fatal")
```

lgr sets up Loggers in a way so that R6 piping with `$` is possible.
This works similar to the magrittr (`#%>#`) pipes.

``` r
lg$
  set_threshold("info")$
  set_appenders(AppenderConsole$new(threshold = "info"))$
  set_propagate(FALSE)
```

### With a list object

``` r
lg$config(list(
  threshold = "info",
  propagate = FALSE,
  appenders = AppenderConsole$new(threshold = "info")
))
#> <Logger> [info] test
#> 
#> appenders:
#>   [[1]]: <AppenderConsole> [info] -> console
```

### With YAML or JSON

You can use YAML and JSON config files with lgr.

``` r
lg$config("path/to/config.yaml")
lg$config("path/to/config.json")
```

You can also pass in YAML/JSON directly as a character string (or vector
with one element per line)

``` r
# Via YAML
cfg <- "
  Logger:
    threshold: info
    propagate: false
    appenders:
      AppenderConsole:
        threshold: info
"
lg$config(cfg)
#> <Logger> [info] test
#> 
#> appenders:
#>   AppenderConsole: <AppenderConsole> [info] -> console
```

## Examples

### Logging to the console

lgr comes with simple but powerful formatting syntax for LogEvents.
Please refer to
[`?format.LogEvent`](https://s-fleck.github.io/lgr/reference/print.LogEvent.md)
for the full list of available placeholders.

``` r
lg <- get_logger("test")
lg$set_appenders(list(cons = AppenderConsole$new()))
lg$set_propagate(FALSE)


lg$info("the default format")
lg$appenders$cons$layout$set_fmt("%L (%n) [%t] %c(): !! %m !!")
lg$info("A more involved custom format")
```

If this is not enough for you, you can use `LayoutGlue` based on the
awesome [glue](https://github.com/tidyverse/glue) package. The syntax is
a bit more verbose, and `AppenderGlue` is a bit less performant than
`AppenderFormat`, but the possibilities are endless.

``` r
# install.packages("glue")
library(glue)
lg$appenders$cons$set_layout(LayoutGlue$new(
  fmt = "{.logger$name} {level_name} {caller}: {toupper(msg)}"
))
lg$info("with glue")
```

All fields of the \[LogEvent\] object are exposed through LayoutGlue, so
please refer to
[`?LogEvent`](https://s-fleck.github.io/lgr/reference/LogEvent.md) for a
list of all available Fields.

### Logging to JSON files

JavaScript Object Notation (JSON) is an open-standard file format that
uses human-readable text to transmit data objects consisting of
attribute–value pairs and array data types
([Wikipedia](https://en.wikipedia.org/wiki/JSON)). JSON is the
**recommended text-based logging format when logging to files**
[¹](#fn1), as it is human- as well as machine readable. You should only
log to a different format if you have very good reasons for it. The
easiest way to log to JSON files is with AppenderJson[²](#fn2)

``` r
# install.packages("jsonlite")
tf <- tempfile()

lg <- get_logger("test")

lg$set_appenders(list(json = AppenderJson$new(file = tf)))
lg$set_propagate(FALSE)

lg$info("JSON naturally ", field = "custom")
lg$info("supports custom", numbers = 1:3)
lg$info("log fields", use = "JSON")
```

JSON is easy to parse and analyse with R. lgr provides the function
[`read_json_lines()`](https://s-fleck.github.io/lgr/reference/read_json_lines.md)
that can be used to read JSON log files, but you can also use
AppenderJson’s `$data` binding for an even more convenient method to
read the logfile.

``` r
lg$appenders$json$data
# same as 
read_json_lines(tf)
```

    #>   level           timestamp logger caller             msg  field numbers  use
    #> 1   400 2026-01-30 13:44:07   test   eval JSON naturally  custom    NULL <NA>
    #> 2   400 2026-01-30 13:44:07   test   eval supports custom   <NA> 1, 2, 3 <NA>
    #> 3   400 2026-01-30 13:44:07   test   eval      log fields   <NA>    NULL JSON

JSON is also human readable, though this vignette does not transport
that fact very well because of the lack of horizontal space.

``` r
lg$appenders$json$show()
# same as
cat(readLines(tf), sep = "\n")
```

    #> {"level":400,"timestamp":"2026-01-30 13:44:07","logger":"test","caller":"eval","msg":"JSON naturally ","field":"custom"}
    #> {"level":400,"timestamp":"2026-01-30 13:44:07","logger":"test","caller":"eval","msg":"supports custom","numbers":[1,2,3]}
    #> {"level":400,"timestamp":"2026-01-30 13:44:07","logger":"test","caller":"eval","msg":"log fields","use":"JSON"}

``` r
# cleanup
lg$config(NULL)
#> <Logger> [all] test
#> 
#> inherited appenders:
#>   console: <AppenderConsole> [info] -> console
unlink(tf)
```

### Logging to rotating files

lgr can also log to rotating files. The following example logs to a file
that is reset and backed-up once it reaches a size of 10kb. Only the
last 5 backups of the logfile are kept.

``` r
# install.packages("rotor")
tf <- tempfile(fileext = ".log")

lg <- get_logger("test")$
  set_propagate(FALSE)$
  set_appenders(list(rotating = AppenderFileRotating$new(
    file = tf, 
    size = "10 kb",
    max_backups = 5))
  )
#> Superclass AppenderFile has cloneable=FALSE, but subclass AppenderFileRotating has cloneable=TRUE. A subclass cannot be cloneable when its superclass is not cloneable, so cloning will be disabled for AppenderFileRotating.

for (i in 1:100) lg$info(paste(LETTERS, sep = "-"))

# display info on the backups of tf
lg$appenders$rotating$backups
#>                                     path             name sfx ext  size isdir
#> 1 /tmp/Rtmp8xTiJj/file1f1d7d4a1a72.1.log file1f1d7d4a1a72   1 log 10608 FALSE
#> 2 /tmp/Rtmp8xTiJj/file1f1d7d4a1a72.2.log file1f1d7d4a1a72   2 log 10608 FALSE
#> 3 /tmp/Rtmp8xTiJj/file1f1d7d4a1a72.3.log file1f1d7d4a1a72   3 log 10608 FALSE
#> 4 /tmp/Rtmp8xTiJj/file1f1d7d4a1a72.4.log file1f1d7d4a1a72   4 log 10608 FALSE
#> 5 /tmp/Rtmp8xTiJj/file1f1d7d4a1a72.5.log file1f1d7d4a1a72   5 log 10608 FALSE
#>   mode               mtime               ctime               atime  uid  gid
#> 1  644 2026-01-30 13:44:08 2026-01-30 13:44:08 2026-01-30 13:44:08 1001 1001
#> 2  644 2026-01-30 13:44:08 2026-01-30 13:44:08 2026-01-30 13:44:08 1001 1001
#> 3  644 2026-01-30 13:44:08 2026-01-30 13:44:08 2026-01-30 13:44:08 1001 1001
#> 4  644 2026-01-30 13:44:08 2026-01-30 13:44:08 2026-01-30 13:44:08 1001 1001
#> 5  644 2026-01-30 13:44:08 2026-01-30 13:44:08 2026-01-30 13:44:08 1001 1001
#>    uname grname index
#> 1 runner runner     1
#> 2 runner runner     2
#> 3 runner runner     3
#> 4 runner runner     4
#> 5 runner runner     5

# manually delete all backups
invisible(lg$appenders$rotating$prune(0))
lg$appenders$rotating$backups
#>  [1] path   name   sfx    ext    size   isdir  mode   mtime  ctime  atime 
#> [11] uid    gid    uname  grname index 
#> <0 rows> (or 0-length row.names)

#cleanup
unlink(tf)
```

### Logger hierarchies

The most common use cases for creating a new Logger rather than just
using the root Logger is if you create a Package that should contain
logging. This way you can have separate Appenders (e.g logfiles) and
thresholds for each package.

``` r
# The logger name should be the same as the package name
tf <- tempfile()
lg <- get_logger("mypackage")
lg$add_appender(AppenderFile$new(tf))  
```

The [`print()`](https://rdrr.io/r/base/print.html) method for Loggers
gives a nice overview of the newly created Logger:

``` r
print(lg)
#> <Logger> [all] mypackage
#> 
#> appenders:
#>   [[1]]: <AppenderFile> [all] -> /tmp/Rtmp8xTiJj/file1f1dda67bf3
#> 
#> inherited appenders:
#>   console: <AppenderConsole> [info] -> console
```

This tells us that `lg` logs all events of at least level `info`. It
does have a single (unnamed) Appender that logs to a temporary file, and
dispatches all LogEvents it creates to the Appenders of the root Logger
(ignoring the threshold and filters of the root Logger, but not of its
Appenders).

We can use `lg$fatal()`, `lg$info()`, etc.. to log messages with this
Logger:

``` r
lg$info("A test message for lg")
```

If we do not want `lg` to dispatch to the root Logger, we can set
`propagate` to `FALSE`.

``` r
lg$set_propagate(FALSE)
```

When we take a look at the Logger again, we now see that it does not
inherit any Appenders anymore

``` r
print(lg)
#> <Logger> [all] mypackage
#> 
#> appenders:
#>   [[1]]: <AppenderFile> [all] -> /tmp/Rtmp8xTiJj/file1f1dda67bf3
```

Consequently, `lg` no longer outputs log messages to he console

``` r
lg$info("Nothing to see here")
```

``` r
# cleanup
lg$config(NULL)
#> <Logger> [all] mypackage
#> 
#> inherited appenders:
#>   console: <AppenderConsole> [info] -> console
unlink(tf)
```

### Buffered logging

The main purpose of AppenderBuffer is to retain LogEvents in memory and
write them to destinations at a later point in time, e.g. when the
Buffer is full and needs to be flushed. For example, if you log to a
remote database you can postpone this costly operation until after your
analysis is finished.

By setting a [filter](#thresholds) as a custom `$should_flush()` method
for an AppenderBuffer, you can define more complex conditions to trigger
flushing. For example, the will output the last 5 LogEvents that
happened before an `error` occurred.

``` r
lg <- get_logger("buffer")

lg$
  set_threshold(NA)$
  set_propagate(FALSE)$
  set_appenders(
    AppenderBuffer$new(
    threshold = NA,
    buffer_size = 5, # can hold 5 events, the 6th will trigger flushing
    flush_on_exit = FALSE,
    flush_on_rotate = FALSE,
    flush_threshold = "error",
    appenders = AppenderConsole$new(threshold = NA)
  ))

# The for loop below stores 8 log events in the Buffer
for (nm in month.name[1:8]) lg$debug("%s", nm)

# An event of level 'error' or 'fatal' triggers flushing of the buffer
lg$error("But the days grow short when you reach September")
```

### Logging to databases

Logging to databases is simple, though a few aspects can be tricky to
configure based on the backend used. For performance reasons database
inserts are buffered by default. This works exactly identical as
described above for AppenderBuffer. If you want to write each LogEvent
directly to the database, just set the buffer size to `0`. As of lgr
0.4.0, database appenders are part of the **lgrExtra** package that has
to be installed separately. **Database logging is still somewhat
experimental**.

``` r
# install.packages("RSQLite")
# install.packages("lgrExtra")
lg <- get_logger("db_logger")
lg$
  set_propagate(FALSE)$
  add_appender(
    name = "db", 
    lgrExtra::AppenderDbi$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      table = "log",
      buffer_size = 2L
    )
  )

lg$info("Logging to databases uses a buffer")
lg$info("As the buffer size is 2, no insert took place till now")
lg$appenders$db$show()

lg$info("Now as the buffer is rotated, all events are output to the db")
lg$appenders$db$show()
```

### Adding default extra fields to messages

By abusing Filters, lgr can modify LogEvents as they are processed. One
example for when this is useful is assigning a grouping identifier to a
series of log calls.

``` r
# setup an example function
clean   <- function() lgr$info("cleaning data")
process <- function() lgr$info("processing data")
output  <- function() lgr$info("outputing data")

analyze <- function(){
  clean()
  process()
  output()
}
```

[`with_log_value()`](https://s-fleck.github.io/lgr/reference/with_log_level.md)
provides a convenient wrapper to inject values into log calls.

``` r
with_log_value(
  list(dataset_id = "dataset1"), 
  analyze()
)
```

An alternative way to achieve the same is to use one of the
preconfigured Filters that come with lgr. This approach is more more
comfortable for use within functions.

``` r
analyze <- function(id = "dataset1"){
  lgr$add_filter(FilterInject$new(dataset_id = id), name = "inject")
  on.exit(lgr$remove_filter("inject"))
  
  clean()
  process()
  output()
}
analyze()
```

The result is the same in both cases:

You can use
[`with_log_level()`](https://s-fleck.github.io/lgr/reference/with_log_level.md)
and `FilterForceLevel` in a similar fashion to modify the log level of
events conveniently.

### Temporarily disable logging

Temporary disabling logging for portions of code is straight forward and
easy with lgr:

``` r
without_logging({
  lgr$warn("Oh Yeah?")
  lgr$fatal("Oh No")
})
```

### Adding a custom logger to a package

If you are a package author, it is good practice to define a separate
logger for your package. This gives users the ability to easily
enable/disable logging on a per-package basis. Loggers must be
initialized in the packages .onLoad hook. You can do this by adding the
following code to any `.R` file inside the `R/` directory of your
package:

``` r
# mypackage/R/mypackage-package.R
.onLoad <- function(...){
  assign(
    "lg",  # the recommended name for a logger object
    lgr::get_logger(name = "mypackage"),  # should be the same as the package name
    envir = parent.env(environment())
  )
}
```

You can also just use
[`lgr::use_logger()`](https://s-fleck.github.io/lgr/reference/use_logger.md)
to generate the appropriate code for your package automatically.

After you set this up you can use `lg$fatal()`, `lg$info()`, etc… inside
your package. You do not have to define any appenders, since all log
events will get redirected to the root Logger (see
[Inheritance](#hierarchy)).

### Creating a Layout that mimics NodeJS Winston logstash

see <https://github.com/s-fleck/lgr/issues/29>

### Adding the source file / R-script to the log event

Getting the source file is sadly non-trivial in R, otherwise it would
have been included in the core logging functions. Luckily, this is easy
to add with filters and the awesome
[this.path](https://CRAN.R-project.org/package=this.path) package. This
solution works in most scenarios, but not in all (for example, building
this vignette).

``` r
# install.packages("this.path")

lg <- get_logger("srcfile")
lg$add_filter(function(event){
  tryCatch({
    event$srcfile <- this.path::this.path()
  }, error = function(e) NULL)
  TRUE
})
```

## References

[Python Logging](https://docs.python.org/3/library/logging.html)

[Eric Stenbock: The True Story of A
Vampire](https://gutenberg.net.au/ebooks06/0606601h.html)

------------------------------------------------------------------------

1.  Technically, the logger does not produce standard JSON files but
    [JSON lines](https://jsonlines.org/)

2.  AppenderJson is just an AppenderFile with LayotJson as default
    Layout and a few extra features
