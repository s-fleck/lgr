# Simple Logging

lgr provides convenience functions managing the root Logger. These are
designed chiefly for interactive use and are less verbose than their R6
method counterparts.

`threshold()` sets or retrieves the threshold for an
[Appender](https://s-fleck.github.io/lgr/reference/Appender.md) or
[Logger](https://s-fleck.github.io/lgr/reference/Logger.md) (the minimum
level of log messages it processes). It's `target` defaults to the root
logger. (equivalent to `lgr::lgr$threshold` and
`lgr::lgr$set_threshold`)

`console_threshold()` is a shortcut to set the threshold of the root
loggers
[AppenderConsole](https://s-fleck.github.io/lgr/reference/AppenderConsole.md),
which is usually the only Appender that manages console output for a
given R session. (equivalent to `lgr::lgr$appenders$console$threshold`
and `lgr::lgr$appenders$console$set_threshold`)

`add_appender()` and `remove_appender()` add Appenders to Loggers and
other Appenders. (equivalent to `lgr::lgr$add_appender` and
`lgr::lgr$remove_appender`)

`show_log()` displays the last `n` log entries of an Appender (or a
Logger with such an Appender attached) with a `$show()` method. Most,
but not all Appenders support this function (try
[AppenderFile](https://s-fleck.github.io/lgr/reference/AppenderFile.md)
or
[AppenderBuffer](https://s-fleck.github.io/lgr/reference/AppenderBuffer.md)).

`show_data()` and `show_dt()` work similar to `show_log()`, except that
they return the log as `data.frame` or `data.table` respectively. Only
Appenders that log to formats that can easily be converted to
`data.frames` are supported (try
[AppenderJson](https://s-fleck.github.io/lgr/reference/AppenderFile.md)
or
[AppenderBuffer](https://s-fleck.github.io/lgr/reference/AppenderBuffer.md)).

The easiest way to try out this features is by adding an AppenderBuffer
to the root logger with
[`basic_config(memory = TRUE)`](https://s-fleck.github.io/lgr/reference/basic_config.md).

## Usage

``` r
log_exception(code, logfun = lgr$fatal, caller = get_caller(-3))

threshold(level, target = lgr::lgr)

console_threshold(level, target = lgr::lgr$appenders$console)

add_appender(appender, name = NULL, target = lgr::lgr)

remove_appender(pos, target = lgr::lgr)

show_log(threshold = NA_integer_, n = 20L, target = lgr::lgr)

show_dt(target = lgr::lgr)

show_data(target = lgr::lgr)
```

## Arguments

- code:

  Any R code

- logfun:

  a `function` for processing the log request, usually `lgr$info()`,
  `lgr$debug()`, etc... .

- caller:

  a `character` scalar. The name of the calling function

- level:

  `integer` or `character` scalar: the desired log level

- target:

  a [Logger](https://s-fleck.github.io/lgr/reference/Logger.md) or
  [Appender](https://s-fleck.github.io/lgr/reference/Appender.md) or the
  name of a Logger as `character` scalar

- appender:

  an `Appender`

- name:

  `character` scalar. An optional name for the new Appender.

- pos:

  `integer` index or `character` names of the appenders to remove

- threshold:

  `character` or `integer` scalar. The minimum [log
  level](https://s-fleck.github.io/lgr/reference/get_log_levels.md) that
  should be processed by the root logger.

- n:

  `integer` scalar. Show only the last `n` log entries that match
  `threshold`

## Value

`threshold()` and `console_threshold()` return the
[log_level](https://s-fleck.github.io/lgr/reference/get_log_levels.md)
of `target` as `integer` (invisibly)

`add_appender()` and `remove_appender()` return `target`.

`show_log()` prints to the console and returns whatever the target
Appender's `$show()` method returns, usually a `character` vector,
`data.frame` or `data.table` (invisibly).

`show_data()` always returns a `data.frame` and `show_dt()` always
returns a `data.table`.

## Examples

``` r
# Get and set the threshold of the root logger
threshold("error")
threshold()
#> [1] 200
lgr$info("this will be supressed")
lgr$error("an important error message")
#> ERROR [13:41:55.908] an important error message

# you can also specify a target to modify other loggers
lg <- get_logger("test")
threshold("fatal", target = lg)
threshold(target = lg)
#> [1] 100

# If a Logger's threshold is not set, the threshold is inherited from
# its parent, in this case the root logger (that we set to error/200 before)
threshold(NULL, target = lg)
threshold(target = lg)
#> [1] 200

# Alternative R6 API for getting/setting thresholds
lg$set_threshold("info")
lg$threshold
#> [1] 400
lg$set_threshold(300)
lg$threshold
#> [1] 300
lg$set_threshold(NULL)
lg$threshold
#> [1] 200

# cleanup
lgr$config(NULL)
#> <LoggerRoot> [info] root
lg$config(NULL)
#> <Logger> [info] test


# add Appenders to a Logger
add_appender(AppenderConsole$new(), "second_console_appender")
lgr$fatal("Multiple console appenders are a bad idea")
#> FATAL [13:41:55.918] Multiple console appenders are a bad idea
remove_appender("second_console_appender")
lgr$info("Good that we defined an appender name, so it's easy to remove")

# Reconfigure the root logger
basic_config(memory = TRUE)
#> <LoggerRoot> [info] root
#> 
#> appenders:
#>   console: <AppenderConsole> [all] -> console
#>   memory : <AppenderBuffer>  [all] -> 0 child Appenders

# log some messages
lgr$info("a log message")
#> INFO  [13:41:55.923] a log message
lgr$info("another message with data", data = 1:3)
#> INFO  [13:41:55.925] another message with data {data: (1, 2, 3)}

show_log()
#> INFO  [13:41:55] a log message
#> INFO  [13:41:55] another message with data
show_data()
#>   level           timestamp logger caller                       msg
#> 1   400 2026-01-30 13:41:55   root   eval             a log message
#> 2   400 2026-01-30 13:41:55   root   eval another message with data
#>                      rawMsg    data
#> 1             a log message    NULL
#> 2 another message with data 1, 2, 3

# cleanup
lgr$config(NULL)
#> <LoggerRoot> [info] root
```
