# Print a Logger Object

The [`print()`](https://rdrr.io/r/base/print.html) method for Loggers
displays the most important aspects of the Logger.

You can also print just the `ancestry` of a Logger which can be accessed
with with `logger$ancestry()`. This returns a named `character` vector
whose names correspond to the names of the Loggers `logger` inherits
from. The `TRUE`/`FALSE` status of its elements correspond to the
`propagate` values of these Loggers.

## Usage

``` r
# S3 method for class 'Logger'
print(x, color = requireNamespace("crayon", quietly = TRUE), ...)

# S3 method for class 'Logger'
format(x, color = FALSE, ...)

# S3 method for class 'ancestry'
print(x, color = requireNamespace("crayon", quietly = TRUE), ...)

# S3 method for class 'ancestry'
format(x, color = FALSE, ...)
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
print(lgr)
#> <LoggerRoot> [info] root
#> 
#> appenders:
#>   console: <AppenderConsole> [all] -> console
# print only the ancestry of a logger
lg <- get_logger("AegonV/Aerys/Rheagar/Aegon")
get_logger("AegonV/Aerys/Rheagar")$set_propagate(FALSE)

print(lg$ancestry)
#> AegonV/Aerys/Rheagar/Aegon 
unclass(lg$ancestry)
#>  AegonV   Aerys Rheagar   Aegon 
#>    TRUE    TRUE   FALSE    TRUE 
```
