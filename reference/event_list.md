# A List of LogEvents

An event_list is a class for
[`list()`](https://rdrr.io/r/base/list.html)s whose only elements are
[LogEvents](https://s-fleck.github.io/lgr/reference/LogEvent.md). This
structure is occasionally used internally in lgr (for example by
[AppenderBuffer](https://s-fleck.github.io/lgr/reference/AppenderBuffer.md))
and can be useful for developers that want to write their own Appenders.

## Usage

``` r
event_list(...)

as_event_list(x, ...)

# S3 method for class 'list'
as_event_list(x, ..., scalarize = FALSE)

# S3 method for class 'LogEvent'
as_event_list(x, ..., scalarize = FALSE)

# S3 method for class 'data.frame'
as_event_list(x, na.rm = TRUE, ...)

as.data.table.event_list(x, na.rm = TRUE)

# S3 method for class 'event_list'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  stringsAsFactors = FALSE,
  na.rm = TRUE,
  ...
)
```

## Arguments

- ...:

  for `event` elements to be added to the list, for the `as_*()`
  functions parameters passed on to methods.

- x:

  any `R` object

- scalarize:

  `logical` scalar. Turn
  [LogEvents](https://s-fleck.github.io/lgr/reference/LogEvent.md) with
  non-scalar `msg` field into separate log events

- na.rm:

  remove `NA` values before coercing a data.frame to an `event_list()`.

- row.names:

  `NULL` or a character vector giving the row names for the data frame.
  Missing values are not allowed.

- optional:

  currently ignored and only included for compatibility.

- stringsAsFactors:

  `logical` scalar: should `character` vectors be converted to factors?
  Defaults to `FALSE` (as opposed to
  [`base::as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html))
  and is only included for compatibility.

## Value

an `event_list()` and `as_event_list()` return a flat `list` of
[LogEvents](https://s-fleck.github.io/lgr/reference/LogEvent.md). Nested
lists get automatically flattened.

`as.data.frame` and `as.data.table` return a `data.frame` or
`data.table` respectively

## Details

For convenience,
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) and
`as.data.table()` methods exist for event lists.

## See also

Other docs relevant for extending lgr:
[`LogEvent`](https://s-fleck.github.io/lgr/reference/LogEvent.md),
[`as_LogEvent()`](https://s-fleck.github.io/lgr/reference/as_LogEvent.md),
[`standardize_threshold()`](https://s-fleck.github.io/lgr/reference/standardize_threshold.md)

## Examples

``` r
e <- LogEvent$new(level = 300, msg = "a", logger = lgr)
as_event_list(e)
#> [[1]]
#> WARN  [2026-01-30 11:47:35] a
#> 
#> attr(,"class")
#> [1] "event_list" "list"      
as_event_list(c(e, e))
#> [[1]]
#> WARN  [2026-01-30 11:47:35] a
#> 
#> [[2]]
#> WARN  [2026-01-30 11:47:35] a
#> 
#> attr(,"class")
#> [1] "event_list" "list"      
# nested lists get automatically unnested
as_event_list(c(e, list(nested_event = e)))
#> [[1]]
#> WARN  [2026-01-30 11:47:35] a
#> 
#> $nested_event
#> WARN  [2026-01-30 11:47:35] a
#> 
#> attr(,"class")
#> [1] "event_list" "list"      

# scalarize = TRUE "unpacks" events with vector log messages
e <- LogEvent$new(level = 300, msg = c("A", "B"), logger = lgr)
as_event_list(e, scalarize = FALSE)
#> [[1]]
#> WARN  [2026-01-30 11:47:35] A
#> WARN  [2026-01-30 11:47:35] B
#> 
#> attr(,"class")
#> [1] "event_list" "list"      
as_event_list(e, scalarize = TRUE)
#> [[1]]
#> WARN  [2026-01-30 11:47:35] A
#> 
#> [[2]]
#> WARN  [2026-01-30 11:47:35] B
#> 
#> attr(,"class")
#> [1] "event_list" "list"      
```
