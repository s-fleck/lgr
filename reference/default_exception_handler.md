# Demote an exception to a warning

Throws a timestamped warning instead of stopping the program. This is
the default exception handler used by
[Loggers](https://s-fleck.github.io/lgr/reference/Logger.md).

## Usage

``` r
default_exception_handler(e)
```

## Arguments

- e:

  an `error condition` object

## Value

The warning as `character` vector

## Examples

``` r
tryCatch(stop("an error has occurred"), error = default_exception_handler)
#> Warning: [2026-01-30 13:43:58.333] NULL ~ error in `doTryCatch(return(expr), name, parentenv, handler)`: an error has occurred
```
