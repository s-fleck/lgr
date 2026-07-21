# Print an Appender object

The [`print()`](https://rdrr.io/r/base/print.html) method for Loggers
displays the most important aspects of the Appender.

## Usage

``` r
# S3 method for class 'Appender'
print(x, color = requireNamespace("crayon", quietly = TRUE), ...)
```

## Arguments

- x:

  any R Object

- color:

  `TRUE` or `FALSE`: Output with color? Requires the Package **crayon**

- ...:

  ignored

## Value

[`print()`](https://rdrr.io/r/base/print.html) returns `x` (invisibly),
[`format()`](https://rdrr.io/r/base/format.html) returns a `character`
vector.

## Examples

``` r
# print most important details of logger
print(lgr$console)
#> NULL
```
