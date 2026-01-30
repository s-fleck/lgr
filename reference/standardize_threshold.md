# Standardize User-Input Log Levels to Their Integer Representation

These are helper functions for verifying log levels and converting them
from their character to their integer representations. This is primarily
useful if you want to build your own
[Loggers](https://s-fleck.github.io/lgr/reference/Logger.md),
[Appenders](https://s-fleck.github.io/lgr/reference/Appender.md) or
[Layouts](https://s-fleck.github.io/lgr/reference/Layout.md) and need to
handle log levels in a way that is consistent with lgr .

## Usage

``` r
standardize_threshold(
  x,
  log_levels = c(getOption("lgr.log_levels"), c(all = NA_integer_, off = 0L))
)

is_threshold(x)

standardize_log_level(x, log_levels = getOption("lgr.log_levels"))

is_log_level(x)

standardize_log_levels(x, log_levels = getOption("lgr.log_levels"))

is_log_levels(x)
```

## Arguments

- x:

  a `character` or `integer` scalar, or vector for
  standardize_log_levels

- log_levels:

  named `integer` vector of valid log levels

## Value

An unnamed `integer` vector

## See also

Other docs relevant for extending lgr:
[`LogEvent`](https://s-fleck.github.io/lgr/reference/LogEvent.md),
[`as_LogEvent()`](https://s-fleck.github.io/lgr/reference/as_LogEvent.md),
[`event_list()`](https://s-fleck.github.io/lgr/reference/event_list.md)

## Examples

``` r
standardize_threshold("info")
#> [1] 400
standardize_threshold("all")
#> [1] NA
is_threshold("all")
#> [1] TRUE
is_threshold("foobar")
#> [1] FALSE

standardize_log_level("info")
#> [1] 400
# all is a valid threshold, but not a valid log level
try(is.na(standardize_log_level("all")))
#> Error in standardize_log_level("all") : 
#>   '"all"' must either the numeric or character representation of one of the following log levels: fatal (100), error (200), warn (300), info (400), debug (500), trace (600)
is_log_level("all")
#> [1] FALSE

# standardized_log_level intentionally only works with scalars, because many
# functions require scalar log level inputs
try(standardize_log_level(c("info", "fatal")))
#> Error : 'c("info", "fatal")' must be a scalar log level

# You can still use standardize_log_levels() (plural) to work with vectors
standardize_log_levels(c("info", "fatal"))
#> [1] 400 100
```
