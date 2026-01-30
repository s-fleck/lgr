# Inject values into all events processed by a Logger/Appender

Inject arbitrary values into all
[LogEvents](https://s-fleck.github.io/lgr/reference/LogEvent.md)
processed by a Logger/Appender. It is recommended to use filters that
modify LogEvents only with Loggers, but they will also work with
Appenders.

## Super class

[`lgr::EventFilter`](https://s-fleck.github.io/lgr/reference/EventFilter.md)
-\> `FilterInject`

## Public fields

- `values`:

  a named `list` of values to be injected into each
  [LogEvent](https://s-fleck.github.io/lgr/reference/LogEvent.md)
  processed by this filter

## Methods

### Public methods

- [`FilterInject$new()`](#method-FilterInject-new)

- [`FilterInject$clone()`](#method-FilterInject-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize a new FilterInject

#### Usage

    FilterInject$new(..., .list = list())

#### Arguments

- `..., .list`:

  any number of named R objects that will be injected as custom fields
  into all
  [LogEvents](https://s-fleck.github.io/lgr/reference/LogEvent.md)
  processed by the Appender/Logger that this filter is attached to. See
  also
  [`with_log_value()`](https://s-fleck.github.io/lgr/reference/with_log_level.md).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    FilterInject$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
lg <- get_logger("test")

analyse <- function(){
  lg$add_filter(FilterInject$new(type = "analysis"), "inject")
  on.exit(lg$remove_filter("inject"))
  lg$error("an error with forced custom 'type'-field")
}

analyse()
#> ERROR [13:43:53.358] an error with forced custom 'type'-field {type: `analysis`}
lg$error("an normal error")
#> ERROR [13:43:53.362] an normal error
lg$config(NULL)  # reset config
#> <Logger> [info] test
#> 
#> inherited appenders:
#>   console: <AppenderConsole> [all] -> console
```
