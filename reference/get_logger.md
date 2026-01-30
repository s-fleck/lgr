# Get/Create a Logger

Get/Create a Logger

## Usage

``` r
get_logger(name, class = Logger, reset = FALSE)

get_logger_glue(name)
```

## Arguments

- name:

  a `character` scalar or vector: The qualified name of the Logger as a
  hierarchical value.

- class:

  An [R6ClassGenerator](https://r6.r-lib.org/reference/R6Class.html)
  object. Usually `Logger` or `LoggerGlue` are the only valid choices.

- reset:

  a `logical` scalar. If `TRUE` the logger is reset to an unconfigured
  state. Unlike `$config(NULL)` this also replaces a `LoggerGlue` with
  vanilla `Logger`. Please note that this will invalidate Logger
  references created before the reset call (see examples).

## Value

a [Logger](https://s-fleck.github.io/lgr/reference/Logger.md)

## Examples

``` r
lg <- get_logger("log/ger/test")
# equivalent to
lg <- get_logger(c("log", "ger", "test"))
lg$warn("a %s message", "warning")
#> WARN  [11:47:35.374] a warning message
lg
#> <Logger> [info] log/ger/test
#> 
#> inherited appenders:
#>   console: <AppenderConsole> [all] -> console
lg$parent
#> <Logger> [info] log/ger
#> 
#> inherited appenders:
#>   console: <AppenderConsole> [all] -> console

if (requireNamespace('glue')){
  lg <- get_logger_glue("log/ger")
}
lg$warn("a {.text} message", .text = "warning")
#> WARN  [11:47:35.380] a warning message

# completely reset 'glue' to an unconfigured vanilla Logger
get_logger("log/ger", reset = TRUE)
#> <Logger> [info] log/ger
#> 
#> inherited appenders:
#>   console: <AppenderConsole> [all] -> console
# WARNING: this invalidates existing references to the Logger
try(lg$info("lg has been invalidated an no longer works"))
#> Warning: [2026-01-30 11:47:35.383] log/ger ~ error in `lg$info("lg has been invalidated an no longer works")`: Trying to log via a Logger reference that is no longer valid. Logger references become invalid when you reset a when you reset a Logger with `get_logger(reset = TRUE)`. Please re-create the Logger reference with with `get_logger(log/ger)`

lg <- get_logger("log/ger")
lg$info("now all is well again")
#> INFO  [11:47:35.385] now all is well again
```
