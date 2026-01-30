# Basic Setup for the Logging System

A quick and easy way to configure the root logger. This is less powerful
then using `lgr$config()` or `lgr$set_*()` (see
[Logger](https://s-fleck.github.io/lgr/reference/Logger.md)), but
reduces the most common configurations to a single line of code.

## Usage

``` r
basic_config(
  file = NULL,
  fmt = "%L [%t] %m",
  timestamp_fmt = "%Y-%m-%d %H:%M:%OS3",
  threshold = "info",
  appenders = NULL,
  console = if (is.null(appenders)) "all" else FALSE,
  console_fmt = "%L [%t] %m %f",
  console_timestamp_fmt = "%H:%M:%OS3",
  console_connection = NULL,
  memory = FALSE
)
```

## Arguments

- file:

  `character` scalar: If not `NULL` a
  [AppenderFile](https://s-fleck.github.io/lgr/reference/AppenderFile.md)
  will be created that logs to this file. If the filename ends in
  `.jsonl`, the Appender will be set up to use the [JSON
  Lines](https://jsonlines.org/) format instead of plain text (see
  [AppenderFile](https://s-fleck.github.io/lgr/reference/AppenderFile.md)
  and
  [AppenderJson](https://s-fleck.github.io/lgr/reference/AppenderFile.md)).

- fmt:

  `character` scalar: Format to use if `file` is supplied and not a
  `.jsonl` file. If `NULL` it defaults to `"%L [%t] %m"` (see
  [format.LogEvent](https://s-fleck.github.io/lgr/reference/print.LogEvent.md))

- timestamp_fmt:

  see [`format.POSIXct()`](https://rdrr.io/r/base/strptime.html)

- threshold:

  `character` or `integer` scalar. The minimum [log
  level](https://s-fleck.github.io/lgr/reference/get_log_levels.md) that
  should be processed by the root logger.

- appenders:

  a single
  [Appender](https://s-fleck.github.io/lgr/reference/Appender.md) or a
  list thereof.

- console:

  `logical` scalar or a `threshold` (see above). Add an appender logs to
  the console (i.e. displays messages in an interactive R session)

- console_fmt:

  `character` scalar: like `fmt` but used for console output

- console_timestamp_fmt:

  `character` scalar: like `timestamp_fmt` but used for console output

- console_connection:

  see [`cat()`](https://rdrr.io/r/base/cat.html) `file` argument.

- memory:

  `logical` scalar. or a `threshold` (see above). Add an Appender that
  logs to a memory buffer, see also
  [`show_log()`](https://s-fleck.github.io/lgr/reference/simple_logging.md)
  and
  [AppenderBuffer](https://s-fleck.github.io/lgr/reference/AppenderBuffer.md)

## Value

the `root` Logger (lgr)

## Examples

``` r
# log to a file
basic_config(file = tempfile())
#> <LoggerRoot> [info] root
#> 
#> appenders:
#>   file   : <AppenderFile>    [all] -> /tmp/RtmpoOsnje/file1b82334ba3fe
#>   console: <AppenderConsole> [all] -> console
unlink(lgr$appenders$file$file)  # cleanup

basic_config(file = tempfile(fileext = "jsonl"))
#> <LoggerRoot> [info] root
#> 
#> appenders:
#>   file   : <AppenderFile>    [all] -> /tmp/RtmpoOsnje/file1b826cc55733jsonl
#>   console: <AppenderConsole> [all] -> console
unlink(lgr$appenders$file$file)  # cleanup

# log debug messages to a memory buffer
basic_config(threshold = "all", memory = "all", console = "info")
#> <LoggerRoot> [all] root
#> 
#> appenders:
#>   console: <AppenderConsole> [info] -> console
#>   memory : <AppenderBuffer>  [ all] -> 0 child Appenders
lgr$info("an info message")
#> INFO  [13:41:52.576] an info message
lgr$debug("a hidden message")
show_log()
#> INFO  [13:41:52] an info message
#> DEBUG [13:41:52] a hidden message

# reset to default config
basic_config()
#> <LoggerRoot> [info] root
#> 
#> appenders:
#>   console: <AppenderConsole> [all] -> console
```
