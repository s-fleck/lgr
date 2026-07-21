# Short string representation for R objects

This is inspired by the python function `repr` and produces a short
string representation of any R object that is suitable for logging and
error messages. It is a generic so you can implement methods for custom
S3 objects.

## Usage

``` r
string_repr(x, width = 32, ...)

# S3 method for class '`function`'
string_repr(x, width = 32L, ...)

# S3 method for class 'data.frame'
string_repr(x, width = 32L, ...)

# S3 method for class 'matrix'
string_repr(x, width = 32L, ...)

# S3 method for class 'numeric'
string_repr(x, width = 32L, ...)

# Default S3 method
string_repr(x, width = 32L, ...)
```

## Arguments

- x:

  Any R object.

- width:

  a scalar integer

- ...:

  passed on to methods

## Value

a `scalar` character

## Examples

``` r
string_repr(iris)
#> [1] "<data.frame 150x5>"
string_repr(LETTERS)
#> [1] "(A, B, C, D, E, F, G, H, I, J..)"
string_repr(LETTERS, 10)
#> [1] "(A, B..)"
```
