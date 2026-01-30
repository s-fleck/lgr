# Loggers

A Logger produces a
[LogEvent](https://s-fleck.github.io/lgr/reference/LogEvent.md) that
contains a log message along with metadata (timestamp, calling function,
...) and dispatches it to one or more
[Appenders](https://s-fleck.github.io/lgr/reference/Appender.md) which
are responsible for the output (console, file, ...) of the event.
**lgr** comes with a single pre-configured Logger called the
`root Logger` that can be accessed via `lgr$<...>`. Instantiation of new
Loggers is done with
[`get_logger()`](https://s-fleck.github.io/lgr/reference/get_logger.md).
It is advisable to instantiate a separate Logger with a descriptive name
for each package/script in which you use lgr.

## Note

If you are a package developer you should define a new Logger for each
package, but you do not need to configure it. The user of the package
should decide how and where to output logging, usually by configuring
the root Logger (new Appenders added/removed, Layouts modified, etc...).

## See also

[glue](https://glue.tidyverse.org/)

[`get_logger()`](https://s-fleck.github.io/lgr/reference/get_logger.md)

## Super class

[`lgr::Filterable`](https://s-fleck.github.io/lgr/reference/Filterable.md)
-\> `Logger`

## Active bindings

- `name`:

  A `character` scalar. The unique name of each logger, which also
  includes the names of its ancestors (separated by `/`).

- `threshold`:

  `integer` scalar. The threshold of the `Logger`, or if it `NULL` the
  threshold it inherits from its closest ancestor with a non-`NULL`
  threshold

- `propagate`:

  A `TRUE` or `FALSE`. Should a Logger propagate events to the Appenders
  of its ancestors?

- `ancestry`:

  A named `logical` vector of containing the propagate value of each
  Logger upper the inheritance tree. The names are the names of the
  appenders. `ancestry` is an S3 class with a custom
  [`format()`](https://rdrr.io/r/base/format.html)/[`print()`](https://rdrr.io/r/base/print.html)
  method, so if you want to use the plain logical vector use
  `unclass(lg$ancestry)`

- `parent`:

  a `Logger`. The direct ancestor of the `Logger`.

- `last_event`:

  The last LogEvent produced by the current Logger

- `appenders`:

  a `list` of all
  [Appenders](https://s-fleck.github.io/lgr/reference/Appender.md) of
  the Logger

- `inherited_appenders`:

  A `list` of all appenders that the Logger inherits from its ancestors

- `exception_handler`:

  a `function`. See `$set_exception_handler` and `$handle_exception`

## Methods

### Public methods

- [`Logger$new()`](#method-Logger-new)

- [`Logger$log()`](#method-Logger-log)

- [`Logger$fatal()`](#method-Logger-fatal)

- [`Logger$error()`](#method-Logger-error)

- [`Logger$warn()`](#method-Logger-warn)

- [`Logger$info()`](#method-Logger-info)

- [`Logger$debug()`](#method-Logger-debug)

- [`Logger$trace()`](#method-Logger-trace)

- [`Logger$list_log()`](#method-Logger-list_log)

- [`Logger$config()`](#method-Logger-config)

- [`Logger$add_appender()`](#method-Logger-add_appender)

- [`Logger$remove_appender()`](#method-Logger-remove_appender)

- [`Logger$handle_exception()`](#method-Logger-handle_exception)

- [`Logger$set_exception_handler()`](#method-Logger-set_exception_handler)

- [`Logger$set_propagate()`](#method-Logger-set_propagate)

- [`Logger$set_threshold()`](#method-Logger-set_threshold)

- [`Logger$set_appenders()`](#method-Logger-set_appenders)

- [`Logger$set_replace_empty()`](#method-Logger-set_replace_empty)

- [`Logger$spawn()`](#method-Logger-spawn)

Inherited methods

- [`lgr::Filterable$add_filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-add_filter)
- [`lgr::Filterable$filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-filter)
- [`lgr::Filterable$remove_filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-remove_filter)
- [`lgr::Filterable$set_filters()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-set_filters)

------------------------------------------------------------------------

### Method `new()`

**Loggers should never be instantiated directly with `Logger$new()`**
but rather via
[`get_logger("name")`](https://s-fleck.github.io/lgr/reference/get_logger.md).
This way new Loggers are registered in a global namespace which ensures
uniqueness and facilitates inheritance between Loggers. If `"name"` does
not exist, a new Logger with that name will be created, otherwise the
function returns a Reference to the existing Logger.

`name` is potentially a `"/"` separated hierarchical value like
`foo/bar/baz`. Loggers further down the hierarchy are descendants of the
loggers above and (by default) inherit `threshold` and `Appenders` from
their ancestors.

#### Usage

    Logger$new(
      name = "(unnamed logger)",
      appenders = list(),
      threshold = NULL,
      filters = list(),
      exception_handler = default_exception_handler,
      propagate = TRUE,
      replace_empty = "<NULL>"
    )

#### Arguments

- `name, appenders, threshold, filters, exception_handler, propagate`:

  See section Active bindings.

------------------------------------------------------------------------

### Method [`log()`](https://rdrr.io/r/base/Log.html)

Log an event.

If `level` passes the Logger's `threshold` a new
[LogEvent](https://s-fleck.github.io/lgr/reference/LogEvent.md) with
`level`, `msg`, `timestamp` and `caller` is created. If the new LogEvent
also passes the Loggers
[Filters](https://s-fleck.github.io/lgr/reference/EventFilter.md), it is
be dispatched to the relevant
[Appenders](https://s-fleck.github.io/lgr/reference/Appender.md).

#### Usage

    Logger$log(level, msg, ..., timestamp = Sys.time(), caller = get_caller(-7))

#### Arguments

- `level`:

  a `character` or `integer` scalar. See
  [log_levels](https://s-fleck.github.io/lgr/reference/get_log_levels.md).

- `msg`:

  `character`. A log message. If unnamed arguments are supplied in
  `...`, `msg` is passed on to
  [`base::sprintf()`](https://rdrr.io/r/base/sprintf.html) (which means
  `"%"` have to be escaped), otherwise `msg` is left as-is.

- `...`:

  *unnamed* arguments in `...` must be `character` scalars and are
  passed to [`base::sprintf()`](https://rdrr.io/r/base/sprintf.html).
  *Named* arguments must have unique names but can be arbitrary R
  objects that are passed to
  [`LogEvent$new()`](https://s-fleck.github.io/lgr/reference/LogEvent.md)
  and will be turned into custom fields.

- `timestamp`:

  [POSIXct](https://rdrr.io/r/base/DateTimeClasses.html). Timestamp of
  the event.

- `caller`:

  a `character` scalar. The name of the calling function.

------------------------------------------------------------------------

### Method `fatal()`

Log an Event fatal priority

#### Usage

    Logger$fatal(msg, ..., caller = get_caller(-8L))

#### Arguments

- `msg, ..., caller`:

  see `$log()`

------------------------------------------------------------------------

### Method `error()`

Log an Event error priority

#### Usage

    Logger$error(msg, ..., caller = get_caller(-8L))

#### Arguments

- `msg, ..., caller`:

  see `$log()`

------------------------------------------------------------------------

### Method `warn()`

Log an Event warn priority

#### Usage

    Logger$warn(msg, ..., caller = get_caller(-8L))

#### Arguments

- `msg, ..., caller`:

  see `$log()`

------------------------------------------------------------------------

### Method `info()`

Log an Event info priority

#### Usage

    Logger$info(msg, ..., caller = get_caller(-8L))

#### Arguments

- `msg, ..., caller`:

  see `$log()`

------------------------------------------------------------------------

### Method [`debug()`](https://rdrr.io/r/base/debug.html)

Log an Event debug priority

#### Usage

    Logger$debug(msg, ..., caller = get_caller(-8L))

#### Arguments

- `msg, ..., caller`:

  see `$log()`

------------------------------------------------------------------------

### Method [`trace()`](https://rdrr.io/r/base/trace.html)

Log an Event trace priority

#### Usage

    Logger$trace(msg, ..., caller = get_caller(-8L))

#### Arguments

- `msg, ..., caller`:

  see `$log()`

------------------------------------------------------------------------

### Method `list_log()`

`list_log()` is a shortcut for `do.call(Logger$log, x)`. See
<https://github.com/s-fleck/joblog> for an R package that leverages this
feature to create custom log event types for tracking the status of cron
jobs.

#### Usage

    Logger$list_log(x)

#### Arguments

- `x`:

  a named `list` that must at least contain the named elements `level`
  and `timestamp`

#### Examples

    lg <- get_logger("test")
    lg$list_log(list(level = 400, msg = "example"))

------------------------------------------------------------------------

### Method `config()`

Load a Logger configuration.

#### Usage

    Logger$config(cfg, file, text, list)

#### Arguments

- `cfg`:

  - a special `list` object with any or all of the the following
    elements: `appenders`, `threshold`, `filters`, `propagate`,
    `exception_handler`,

  - the path to a `YAML`/`JSON` config file,

  - a `character` scalar containing `YAML/JSON`,

  - `NULL` (to reset the logger config to the default/unconfigured
    state)

- `file, text, list`:

  can be used as an alternative to `cfg` that enforces that the supplied
  argument is of the specified type. See
  [logger_config](https://s-fleck.github.io/lgr/reference/logger_config.md)
  for details.

------------------------------------------------------------------------

### Method [`add_appender()`](https://s-fleck.github.io/lgr/reference/simple_logging.md)

Add an Appender to the Logger

#### Usage

    Logger$add_appender(appender, name = NULL)

#### Arguments

- `appender`:

  a single
  [Appender](https://s-fleck.github.io/lgr/reference/Appender.md)

- `name`:

  a `character` scalar. Optional but recommended.

#### Examples

    lg <- get_logger("test")
    lg$add_appender(AppenderConsole$new(), name = "myconsole")
    lg$appenders[[1]]
    lg$appenders$myconsole
    lg$remove_appender("myconsole")
    lg$config(NULL)  # reset config

------------------------------------------------------------------------

### Method [`remove_appender()`](https://s-fleck.github.io/lgr/reference/simple_logging.md)

remove an appender

#### Usage

    Logger$remove_appender(pos)

#### Arguments

- `pos`:

  `integer` index or `character` name of the Appender(s) to remove

------------------------------------------------------------------------

### Method `handle_exception()`

To prevent errors in the logging logic from crashing the whole script,
Loggers pass errors they encounter to an exception handler. The default
behaviour is to demote errors to
[warnings](https://rdrr.io/r/base/warnings.html). See also
`set_exception_handler()`.

#### Usage

    Logger$handle_exception(expr)

#### Arguments

- `expr`:

  expression to be evaluated.

------------------------------------------------------------------------

### Method `set_exception_handler()`

Set the exception handler of a logger

#### Usage

    Logger$set_exception_handler(fun)

#### Arguments

- `fun`:

  a `function` with the single argument `e` (an error
  [condition](https://rdrr.io/r/base/conditions.html))

#### Examples

    lgr$info(stop("this produces a warning instead of an error"))

------------------------------------------------------------------------

### Method `set_propagate()`

Should a Logger propagate events to the Appenders of its ancestors?

#### Usage

    Logger$set_propagate(x)

#### Arguments

- `x`:

  `TRUE` or `FALSE`. Should
  [LogEvents](https://s-fleck.github.io/lgr/reference/LogEvent.md) be
  passed on to the appenders of the ancestral Loggers?

------------------------------------------------------------------------

### Method `set_threshold()`

Set the minimum log level of events that a Logger should process

#### Usage

    Logger$set_threshold(level)

#### Arguments

- `level`:

  `character` or `integer` scalar. The minimum [log
  level](https://s-fleck.github.io/lgr/reference/get_log_levels.md) that
  triggers this Logger

------------------------------------------------------------------------

### Method `set_appenders()`

Set the Logger's Appenders

#### Usage

    Logger$set_appenders(x)

#### Arguments

- `x`:

  single [Appender](https://s-fleck.github.io/lgr/reference/Appender.md)
  or a `list` thereof. Appenders control the output of a Logger. Be
  aware that a Logger also inherits the Appenders of its ancestors (see
  [`vignette("lgr", package = "lgr")`](https://s-fleck.github.io/lgr/articles/lgr.md)
  for more info about Logger inheritance).

------------------------------------------------------------------------

### Method `set_replace_empty()`

Set the replacement for empty values (`NULL` or empty vectors)

#### Usage

    Logger$set_replace_empty(x)

#### Arguments

- `x`:

  should be a `character` vector, but other types of values are
  supported. use wisely.

------------------------------------------------------------------------

### Method `spawn()`

Spawn a child Logger. This is very similar to using
[`get_logger()`](https://s-fleck.github.io/lgr/reference/get_logger.md),
but can be useful in some cases where Loggers are created
programmatically

#### Usage

    Logger$spawn(name)

#### Arguments

- `name`:

  `character` vector. Name of the child logger
  `get_logger("foo/bar")$spawn("baz")` is equivalent to
  `get_logger("foo/bar/baz")`

## Examples

``` r
# lgr::lgr is the root logger that is always available
lgr$info("Today is a good day")
#> INFO  [13:43:55.275] Today is a good day
lgr$fatal("This is a serious error")
#> FATAL [13:43:55.276] This is a serious error

# Loggers use sprintf() for string formatting by default
lgr$info("Today is %s", Sys.Date() )
#> INFO  [13:43:55.277] Today is 2026-01-30

# If no unnamed `...` are present, msg is not passed through sprintf()
lgr$fatal("100% bad")  # so this works
#> FATAL [13:43:55.279] 100% bad
lgr$fatal("%s%% bad", 100)  # if you use unnamed arguments, you must escape %
#> FATAL [13:43:55.280] 100% bad

# You can create new loggers with get_logger()
tf <- tempfile()
lg <- get_logger("mylogger")$set_appenders(AppenderFile$new(tf))

# The new logger passes the log message on to the appenders of its parent
# logger, which is by default the root logger. This is why the following
# writes not only the file 'tf', but also to the console.
lg$fatal("blubb")
#> FATAL [13:43:55.283] blubb
readLines(tf)
#> [1] "FATAL [2026-01-30 13:43:55.283] blubb"

# This logger's print() method depicts this relationship.
child <- get_logger("lg/child")
print(child)
#> <Logger> [info] lg/child
#> 
#> inherited appenders:
#>   console: <AppenderConsole> [all] -> console
print(child$name)
#> [1] "lg/child"

# use formatting strings and custom fields
tf2 <- tempfile()
lg$add_appender(AppenderFile$new(tf2, layout = LayoutJson$new()))
lg$info("Not all %s support custom fields", "appenders", type = "test")
#> INFO  [13:43:55.290] Not all appenders support custom fields {type: `test`}
cat(readLines(tf), sep = "\n")
#> FATAL [2026-01-30 13:43:55.283] blubb
#> INFO  [2026-01-30 13:43:55.290] Not all appenders support custom fields {"type":"test"}
cat(readLines(tf2), sep = "\n")
#> {"level":400,"timestamp":"2026-01-30 13:43:55","logger":"mylogger","caller":"eval","msg":"Not all appenders support custom fields","type":"test"}

# cleanup
unlink(c(tf, tf2))
lg$config(NULL)  # reset logger config
#> <Logger> [info] mylogger
#> 
#> inherited appenders:
#>   console: <AppenderConsole> [all] -> console

# LoggerGlue
# You can also create a new logger that uses the awesome glue library for
# string formatting instead of sprintf

if (requireNamespace("glue")){

  lg <- get_logger_glue("glue")
  lg$fatal("blah ", "fizz is set to: {fizz}", foo = "bar", fizz = "buzz")
  # prevent creation of custom fields with prefixing a dot
  lg$fatal("blah ", "fizz is set to: {.fizz}", foo = "bar", .fizz = "buzz")

  #' # completely reset 'glue' to an unconfigured vanilla Logger
  get_logger("glue", reset = TRUE)

}
#> FATAL [13:43:55.314] blah fizz is set to: buzz {foo: `bar`, fizz: `buzz`}
#> FATAL [13:43:55.345] blah fizz is set to: buzz {foo: `bar`}
#> <Logger> [info] glue
#> 
#> inherited appenders:
#>   console: <AppenderConsole> [all] -> console


# Configuring a Logger
lg <- get_logger("test")
lg$config(NULL)  # resets logger to unconfigured state
#> <Logger> [info] test
#> 
#> inherited appenders:
#>   console: <AppenderConsole> [all] -> console

# With setters
lg$
  set_threshold("error")$
  set_propagate(FALSE)$
  set_appenders(AppenderConsole$new(threshold = "info"))

lg$config(NULL)
#> <Logger> [info] test
#> 
#> inherited appenders:
#>   console: <AppenderConsole> [all] -> console

# With a list
lg$config(list(
  threshold = "error",
  propagate = FALSE,
  appenders = list(AppenderConsole$new(threshold = "info"))
))
#> <Logger> [error] test
#> 
#> appenders:
#>   [[1]]: <AppenderConsole> [info] -> console

lg$config(NULL)  # resets logger to unconfigured state
#> <Logger> [info] test
#> 
#> inherited appenders:
#>   console: <AppenderConsole> [all] -> console

# Via YAML
cfg <- "
Logger:
  threshold: error
  propagate: false
  appenders:
    AppenderConsole:
      threshold: info
"

lg$config(cfg)
#> <Logger> [error] test
#> 
#> appenders:
#>   AppenderConsole: <AppenderConsole> [info] -> console
lg$config(NULL)
#> <Logger> [info] test
#> 
#> inherited appenders:
#>   console: <AppenderConsole> [all] -> console

## ------------------------------------------------
## Method `Logger$list_log`
## ------------------------------------------------

lg <- get_logger("test")
lg$list_log(list(level = 400, msg = "example"))
#> INFO  [13:43:55.360] example

## ------------------------------------------------
## Method `Logger$add_appender`
## ------------------------------------------------

lg <- get_logger("test")
lg$add_appender(AppenderConsole$new(), name = "myconsole")
lg$appenders[[1]]
#> <AppenderConsole> [all]
#>   layout: <LayoutFormat> %L [%t] %m %f
#>   destination: console
lg$appenders$myconsole
#> <AppenderConsole> [all]
#>   layout: <LayoutFormat> %L [%t] %m %f
#>   destination: console
lg$remove_appender("myconsole")
lg$config(NULL)  # reset config
#> <Logger> [info] test
#> 
#> inherited appenders:
#>   console: <AppenderConsole> [all] -> console

## ------------------------------------------------
## Method `Logger$set_exception_handler`
## ------------------------------------------------

lgr$info(stop("this produces a warning instead of an error"))
#> Warning: [2026-01-30 13:43:55.375] root ~ error in `lgr$info(stop("this produces a warning instead of an error"))`: this produces a warning instead of an error
```
