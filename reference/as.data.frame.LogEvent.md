# Coerce LogEvents to Data Frames

Coerce LogEvents to `data.frames`,
[`data.tables`](https://rdrr.io/pkg/data.table/man/data.table.html), or
[`tibbles`](https://tibble.tidyverse.org/reference/tibble.html).

## Usage

``` r
# S3 method for class 'LogEvent'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  stringsAsFactors = FALSE,
  ...,
  box_if = function(.) !(is.atomic(.) && identical(length(.), 1L)),
  cols_expand = NULL
)

as.data.table.LogEvent(
  x,
  ...,
  box_if = function(.) !(is.atomic(.) && identical(length(.), 1L)),
  cols_expand = "msg"
)

as_tibble.LogEvent(
  x,
  ...,
  box_if = function(.) !(is.atomic(.) && identical(length(.), 1L)),
  cols_expand = "msg"
)
```

## Arguments

- x:

  any R object.

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

- ...:

  passed on to [`data.frame()`](https://rdrr.io/r/base/data.frame.html)

- box_if:

  a `function` that returns `TRUE` or `FALSE` to determine which values
  are to be boxed (i.e. placed as single elements in a list column). See
  example

- cols_expand:

  `character` vector. Columns to *not* box (even if `box_if()` returns
  `TRUE`). Vectors in these columns will result in multiple rows in the
  result (rather than a single list-column row). This defaults to
  `"msg"` for vectorized logging over the log message.

## See also

[data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html),
[tibble::tibble](https://tibble.tidyverse.org/reference/tibble.html)

## Examples

``` r
lg <- get_logger("test")
lg$info("lorem ipsum")
#> INFO  [11:47:33.974] lorem ipsum
as.data.frame(lg$last_event)
#>   level           timestamp logger caller         msg      rawMsg
#> 1   400 2026-01-30 11:47:33   test   eval lorem ipsum lorem ipsum

lg$info("LogEvents can store any custom log values", df = iris)
#> INFO  [11:47:33.978] LogEvents can store any custom log values {df: <data.frame 150x5>}
as.data.frame(lg$last_event)
#>   level           timestamp logger caller
#> 1   400 2026-01-30 11:47:33   test   eval
#>                                         msg
#> 1 LogEvents can store any custom log values
#>                                      rawMsg           df
#> 1 LogEvents can store any custom log values c(5.1, 4....
head(as.data.frame(lg$last_event)$df[[1]])
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1          5.1         3.5          1.4         0.2  setosa
#> 2          4.9         3.0          1.4         0.2  setosa
#> 3          4.7         3.2          1.3         0.2  setosa
#> 4          4.6         3.1          1.5         0.2  setosa
#> 5          5.0         3.6          1.4         0.2  setosa
#> 6          5.4         3.9          1.7         0.4  setosa

# how boxing works

# by default non-scalars are boxed
lg$info("letters", letters = letters)
#> INFO  [11:47:33.984] letters {letters: (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z)}
as.data.frame(lg$last_event)
#>   level           timestamp logger caller     msg  rawMsg      letters
#> 1   400 2026-01-30 11:47:33   test   eval letters letters a, b, c,....

# this behaviour can be modified by supplying a custom boxing function
as.data.frame(lg$last_event, box_if = function(.) FALSE)
#>    level           timestamp logger caller     msg  rawMsg letters
#> 1    400 2026-01-30 11:47:33   test   eval letters letters       a
#> 2    400 2026-01-30 11:47:33   test   eval letters letters       b
#> 3    400 2026-01-30 11:47:33   test   eval letters letters       c
#> 4    400 2026-01-30 11:47:33   test   eval letters letters       d
#> 5    400 2026-01-30 11:47:33   test   eval letters letters       e
#> 6    400 2026-01-30 11:47:33   test   eval letters letters       f
#> 7    400 2026-01-30 11:47:33   test   eval letters letters       g
#> 8    400 2026-01-30 11:47:33   test   eval letters letters       h
#> 9    400 2026-01-30 11:47:33   test   eval letters letters       i
#> 10   400 2026-01-30 11:47:33   test   eval letters letters       j
#> 11   400 2026-01-30 11:47:33   test   eval letters letters       k
#> 12   400 2026-01-30 11:47:33   test   eval letters letters       l
#> 13   400 2026-01-30 11:47:33   test   eval letters letters       m
#> 14   400 2026-01-30 11:47:33   test   eval letters letters       n
#> 15   400 2026-01-30 11:47:33   test   eval letters letters       o
#> 16   400 2026-01-30 11:47:33   test   eval letters letters       p
#> 17   400 2026-01-30 11:47:33   test   eval letters letters       q
#> 18   400 2026-01-30 11:47:33   test   eval letters letters       r
#> 19   400 2026-01-30 11:47:33   test   eval letters letters       s
#> 20   400 2026-01-30 11:47:33   test   eval letters letters       t
#> 21   400 2026-01-30 11:47:33   test   eval letters letters       u
#> 22   400 2026-01-30 11:47:33   test   eval letters letters       v
#> 23   400 2026-01-30 11:47:33   test   eval letters letters       w
#> 24   400 2026-01-30 11:47:33   test   eval letters letters       x
#> 25   400 2026-01-30 11:47:33   test   eval letters letters       y
#> 26   400 2026-01-30 11:47:33   test   eval letters letters       z
as.data.frame(lg$last_event, cols_expand = "letters")
#>    level           timestamp logger caller     msg  rawMsg letters
#> 1    400 2026-01-30 11:47:33   test   eval letters letters       a
#> 2    400 2026-01-30 11:47:33   test   eval letters letters       b
#> 3    400 2026-01-30 11:47:33   test   eval letters letters       c
#> 4    400 2026-01-30 11:47:33   test   eval letters letters       d
#> 5    400 2026-01-30 11:47:33   test   eval letters letters       e
#> 6    400 2026-01-30 11:47:33   test   eval letters letters       f
#> 7    400 2026-01-30 11:47:33   test   eval letters letters       g
#> 8    400 2026-01-30 11:47:33   test   eval letters letters       h
#> 9    400 2026-01-30 11:47:33   test   eval letters letters       i
#> 10   400 2026-01-30 11:47:33   test   eval letters letters       j
#> 11   400 2026-01-30 11:47:33   test   eval letters letters       k
#> 12   400 2026-01-30 11:47:33   test   eval letters letters       l
#> 13   400 2026-01-30 11:47:33   test   eval letters letters       m
#> 14   400 2026-01-30 11:47:33   test   eval letters letters       n
#> 15   400 2026-01-30 11:47:33   test   eval letters letters       o
#> 16   400 2026-01-30 11:47:33   test   eval letters letters       p
#> 17   400 2026-01-30 11:47:33   test   eval letters letters       q
#> 18   400 2026-01-30 11:47:33   test   eval letters letters       r
#> 19   400 2026-01-30 11:47:33   test   eval letters letters       s
#> 20   400 2026-01-30 11:47:33   test   eval letters letters       t
#> 21   400 2026-01-30 11:47:33   test   eval letters letters       u
#> 22   400 2026-01-30 11:47:33   test   eval letters letters       v
#> 23   400 2026-01-30 11:47:33   test   eval letters letters       w
#> 24   400 2026-01-30 11:47:33   test   eval letters letters       x
#> 25   400 2026-01-30 11:47:33   test   eval letters letters       y
#> 26   400 2026-01-30 11:47:33   test   eval letters letters       z

# The `msg` argument of a log event is always vectorized
lg$info(c("a vectorized", "log message"))
#> INFO  [11:47:33.991] a vectorized
#> INFO  [11:47:33.991] log message
as.data.frame(lg$last_event)
#>   level    timestamp logger caller msg.c..a.vectorized....log.message..
#> 1   400 2026-01-....   test   eval                         a vectorized
#> 2   400 2026-01-....   test   eval                          log message
#>   msg.c..a.vectorized....log.message...1       rawMsg
#> 1                           a vectorized a vector....
#> 2                            log message a vector....

lg$config(NULL)
#> <Logger> [info] test
#> 
#> inherited appenders:
#>   console: <AppenderConsole> [all] -> console
```
