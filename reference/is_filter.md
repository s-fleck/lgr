# Check if an R Object is a Filter

Returns `TRUE` for any R object that can be used as a Filter for
[Loggers](https://s-fleck.github.io/lgr/reference/Logger.md) or,
[Appenders](https://s-fleck.github.io/lgr/reference/Appender.md):

- a `function` with the single argument `event`;

- an
  [EventFilter](https://s-fleck.github.io/lgr/reference/EventFilter.md)
  [R6::R6](https://r6.r-lib.org/reference/R6Class.html) object; or

- any object with a `$filter(event)` method.

**Note:** A Filter **must** return a scalar `TRUE` or `FALSE`, but this
property cannot be checked by `is_filter()`.

## Usage

``` r
is_filter(x)
```

## Arguments

- x:

  any R Object

## Value

`TRUE` or `FALSE`

## See also

[EventFilter](https://s-fleck.github.io/lgr/reference/EventFilter.md),
[Filterable](https://s-fleck.github.io/lgr/reference/Filterable.md)
