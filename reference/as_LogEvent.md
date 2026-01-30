# Coerce objects to LogEvent

Smartly coerce R objects that look like LogEvents to LogEvents. Mainly
useful for developing Appenders.

## Usage

``` r
as_LogEvent(x, ...)

# S3 method for class 'list'
as_LogEvent(x, ...)

# S3 method for class 'data.frame'
as_LogEvent(x, ...)
```

## Arguments

- x:

  any supported R object

- ...:

  currently ignored

## Value

a [LogEvent](https://s-fleck.github.io/lgr/reference/LogEvent.md)

## Details

**Note**: `as_LogEvent.data.frame()` only supports single-row
`data.frames`

## See also

Other docs relevant for extending lgr:
[`LogEvent`](https://s-fleck.github.io/lgr/reference/LogEvent.md),
[`event_list()`](https://s-fleck.github.io/lgr/reference/event_list.md),
[`standardize_threshold()`](https://s-fleck.github.io/lgr/reference/standardize_threshold.md)
