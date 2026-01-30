# Print or Format Logging Data

Print or Format Logging Data

## Usage

``` r
# S3 method for class 'LogEvent'
print(
  x,
  fmt = "%L [%t] %m  %f",
  timestamp_fmt = "%Y-%m-%d %H:%M:%S",
  colors = getOption("lgr.colors"),
  log_levels = getOption("lgr.log_levels"),
  pad_levels = "right",
  excluded_fields = NULL,
  ...
)

# S3 method for class 'LogEvent'
format(
  x,
  fmt = "%L [%t] %m  %f",
  timestamp_fmt = "%Y-%m-%d %H:%M:%S",
  colors = NULL,
  log_levels = getOption("lgr.log_levels"),
  pad_levels = "right",
  excluded_fields = NULL,
  ...
)
```

## Arguments

- x:

  a [LogEvent](https://s-fleck.github.io/lgr/reference/LogEvent.md)

- fmt:

  A `character` scalar that may contain any of the tokens listed bellow
  in the section Format Tokens.

- timestamp_fmt:

  see [`format.POSIXct()`](https://rdrr.io/r/base/strptime.html)

- colors:

  A `list` of `functions` that will be used to color the log levels
  (likely from
  [crayon::crayon](http://r-lib.github.io/crayon/reference/crayon.md)).

- log_levels:

  named `integer` vector of valid log levels

- pad_levels:

  `right`, `left` or `NULL`. Whether or not to pad the log level names
  to the same width on the left or right side, or not at all.

- excluded_fields:

  a `character` vector of fields to exclude from `%j` and `%f`

- ...:

  ignored

## Value

`x` for [`print()`](https://rdrr.io/r/base/print.html) and a `character`
scalar for [`format()`](https://rdrr.io/r/base/format.html)

## Format Tokens

- `%t`:

  The timestamp of the message, formatted according to `timestamp_fmt`)

- `%l`:

  the log level, lowercase `character` representation

- `%L`:

  the log level, uppercase `character` representation

- `%k`:

  the log level, first letter of lowercase `character` representation

- `%K`:

  the log level, first letter of uppercase `character` representation

- `%n`:

  the log level, `integer` representation

- `%g`:

  the name of the logger

- `%p`:

  the PID (process ID). Useful when logging code that uses multiple
  threads.

- `%c`:

  the calling function

- `%m`:

  the log message

- `%r`:

  the raw log message (without string interpolation)

- `%f`:

  all custom fields of `x` in a pseudo-JSON like format that is
  optimized for human readability and console output

- `%j`:

  all custom fields of `x` in proper JSON. This requires that you have
  **jsonlite** installed and does not support colors as opposed to `%f`

## Examples

``` r
# standard fields can be printed using special tokens
x <- LogEvent$new(
  level = 300, msg = "a test event", caller = "testfun()", logger = lgr
)
print(x)
#> WARN  [2026-01-30 11:47:36] a test event
print(x, fmt = c("%t (%p) %c: %n - %m"))
#> 2026-01-30 11:47:36 (7011) testfun(): 300 - a test event
print(x, colors = NULL)
#> WARN  [2026-01-30 11:47:36] a test event

# custom values
y <- LogEvent$new(
  level = 300, msg = "a gps track", logger = lgr,
  waypoints = 10, location = "Austria"
)

# default output with %f
print(y)
#> WARN  [2026-01-30 11:47:36] a gps track  {waypoints: `10`, location: `Austria`}

# proper JSON output with %j
if (requireNamespace("jsonlite")){
print(y, fmt = "%L [%t] %m  %j")
}
#> WARN  [2026-01-30 11:47:36] a gps track  {"waypoints":10,"location":"Austria"}
```
