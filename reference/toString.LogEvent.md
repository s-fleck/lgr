# Convert a LogEvent to a character string

Convert a LogEvent to a character string

## Usage

``` r
# S3 method for class 'LogEvent'
toString(x, ...)
```

## Arguments

- x:

  a [LogEvent](https://s-fleck.github.io/lgr/reference/LogEvent.md)

- ...:

  ignored

## Value

a `character` scalar

## Examples

``` r
toString(LogEvent$new(logger = lgr::lgr))
#> [1] "$level: `400`, $timestamp: `2026-01-30 13:41:57.474235`, $logger: `root`, $caller: `NA`, $msg: `NA`, $rawMsg: `NA`"
```
