# Event Filters

EventFilters specify arbitrarily complex logic for whether or not a
LogEvent should be processed by a
[Logger](https://s-fleck.github.io/lgr/reference/Logger.md) or
[Appender](https://s-fleck.github.io/lgr/reference/Appender.md). They
are attached to Loggers/Appenders via their `$set_filter()` or
`$add_filter()` methods. If any EventFilter evaluates to `FALSE` for a
given event, that event is ignored - similarly to when it does not pass
the objects' threshold.

Usually you do not need to instantiate a formal `EventFilter` object as
you can just use any `function` that has the single argument `event`
instead. If you need to implement more complex filter logic - for
example a filter that is dependent on a dataset - it might be desirable
to subclass EventFilter, as
[R6::R6](https://r6.r-lib.org/reference/R6Class.html) objects can store
data and functions together.

`.obj()` is a special function that can only be used within the
`$filter()` methods of EventFilters. It returns the
[Logger](https://s-fleck.github.io/lgr/reference/Logger.md) or
[Appender](https://s-fleck.github.io/lgr/reference/Appender.md) that the
EventFilter is attached to.

## Usage

``` r
.obj()
```

## Modifying LogEvents with EventFilters

Since LogEvents are R6 objects with reference semantics, EventFilters
can be abused to modify events before passing them on. lgr comes with a
few preset filters that use this property:
[FilterInject](https://s-fleck.github.io/lgr/reference/FilterInject.md)
(similar to
[`with_log_level()`](https://s-fleck.github.io/lgr/reference/with_log_level.md))
and
[FilterForceLevel](https://s-fleck.github.io/lgr/reference/FilterForceLevel.md)
(similar to
[`with_log_value()`](https://s-fleck.github.io/lgr/reference/with_log_level.md)).

**NOTE:** The base class for Filters is called `EventFilter` so that it
doesn't conflict with
[`base::Filter()`](https://rdrr.io/r/base/funprog.html). The recommended
convention for Filter subclasses is to call them `FilterSomething` and
leave out the `Event` prefix.

## See also

[`is_filter()`](https://s-fleck.github.io/lgr/reference/is_filter.md)

## Methods

### Public methods

- [`EventFilter$new()`](#method-EventFilter-new)

- [`EventFilter$clone()`](#method-EventFilter-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize a new EventFilter

#### Usage

    EventFilter$new(fun = function(event) TRUE)

#### Arguments

- `fun`:

  a `function` with a single argument `event` that must return either
  `TRUE` or `FALSE`. Any non-`FALSE` will be interpreted as `TRUE` (= no
  filtering takes place) and a warning will be thrown.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    EventFilter$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
lg <- get_logger("test")
f <- function(event) {
  cat("via event$.logger:", event$.logger$threshold, "\n")  #  works for loggers only
  cat("via .obj():      ",.obj()$threshold, "\n") # works for loggers and appenders
  TRUE
}
lg$add_filter(f)
lg$fatal("test")
#> via event$.logger: 400 
#> via .obj():       400 
#> FATAL [11:27:34.246] test
lg$config(NULL)
#> <Logger> [info] test
#> 
#> inherited appenders:
#>   console: <AppenderConsole> [all] -> console
```
