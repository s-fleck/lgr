# Information About the System

`get_caller()` Tries to determine the calling functions based on
`where`.

## Usage

``` r
get_caller(where = -1L)

get_user(fallback = "unknown user")
```

## Arguments

- where:

  `integer` scalar (usually negative). Look up that many frames up the
  call stack

- fallback:

  A fallback in case the user name could not be determined

## Value

a `character` scalar.

## See also

[`base::sys.call()`](https://rdrr.io/r/base/sys.parent.html)

[`whoami::whoami()`](https://rdrr.io/pkg/whoami/man/whoami.html)

## Examples

``` r
foo <- function() get_caller(-1L)
foo()
#> [1] "foo"
get_user()
#> [1] "unknown user"
```
