# Override the log level of all events processed by a Logger/Appender

Overrides the log level of the Appender/Logger that this filter is
attached to to with `level`. See also
[`with_log_level()`](https://s-fleck.github.io/lgr/reference/with_log_level.md).
It is recommended to use filters that modify LogEvents only with
Loggers, but they will also work with Appenders.

## Super class

[`lgr::EventFilter`](https://s-fleck.github.io/lgr/reference/EventFilter.md)
-\> `FilterForceLevel`

## Public fields

- `level`:

  an `integer` [log
  level](https://s-fleck.github.io/lgr/reference/get_log_levels.md) used
  to override the log levels of each
  [LogEvent](https://s-fleck.github.io/lgr/reference/LogEvent.md)
  processed by this filter.

## Methods

### Public methods

- [`FilterForceLevel$new()`](#method-FilterForceLevel-new)

- [`FilterForceLevel$clone()`](#method-FilterForceLevel-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize a new FilterForceLevel

#### Usage

    FilterForceLevel$new(level)

#### Arguments

- `level`:

  an `integer` or `character` [log
  level](https://s-fleck.github.io/lgr/reference/get_log_levels.md)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    FilterForceLevel$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
lg <- get_logger("test")

analyse <- function(){
  lg$add_filter(FilterForceLevel$new("info"), "force")
  on.exit(lg$remove_filter("force"))
  lg$error("an error with forced log level INFO")
}

analyse()
#> INFO  [13:43:53.188] an error with forced log level INFO
lg$error("an normal error")
#> ERROR [13:43:53.189] an normal error
lg$config(NULL)  # reset config
#> <Logger> [info] test
#> 
#> inherited appenders:
#>   console: <AppenderConsole> [all] -> console
```
