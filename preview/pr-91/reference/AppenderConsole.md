# Log to the console

An Appender that outputs to the R console. If you have the package
**crayon** installed log levels will be coloured by default (but you can
modify this behaviour by passing a custom
[Layout](https://s-fleck.github.io/lgr/reference/Layout.md)).

## See also

[LayoutFormat](https://s-fleck.github.io/lgr/reference/LayoutFormat.md)

Other Appenders:
[`Appender`](https://s-fleck.github.io/lgr/reference/Appender.md),
[`AppenderBuffer`](https://s-fleck.github.io/lgr/reference/AppenderBuffer.md),
[`AppenderFile`](https://s-fleck.github.io/lgr/reference/AppenderFile.md),
[`AppenderFileRotating`](https://s-fleck.github.io/lgr/reference/AppenderFileRotating.md),
[`AppenderFileRotatingDate`](https://s-fleck.github.io/lgr/reference/AppenderFileRotatingDate.md),
[`AppenderFileRotatingTime`](https://s-fleck.github.io/lgr/reference/AppenderFileRotatingTime.md),
[`AppenderTable`](https://s-fleck.github.io/lgr/reference/AppenderTable.md)

## Super classes

[`Filterable`](https://s-fleck.github.io/lgr/reference/Filterable.md)
-\> [`Appender`](https://s-fleck.github.io/lgr/reference/Appender.md)
-\> `AppenderConsole`

## Active bindings

- `connection`:

  The connection that this Appender writes to. See `$set_connection()`.

- `destination`:

  The output destination of the `Appender` in human-readable form. This
  is mainly used when printing information about the Appender itself.

## Methods

### Public methods

- [`AppenderConsole$new()`](#method-AppenderConsole-initialize)

- [`AppenderConsole$append()`](#method-AppenderConsole-append)

- [`AppenderConsole$set_connection()`](#method-AppenderConsole-set_connection)

Inherited methods

- [`Filterable$add_filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-add_filter)
- [`Filterable$filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-filter)
- [`Filterable$remove_filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-remove_filter)
- [`Filterable$set_filters()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-set_filters)
- [`Appender$format()`](https://s-fleck.github.io/lgr/reference/Appender.html#method-format)
- [`Appender$set_layout()`](https://s-fleck.github.io/lgr/reference/Appender.html#method-set_layout)
- [`Appender$set_threshold()`](https://s-fleck.github.io/lgr/reference/Appender.html#method-set_threshold)

------------------------------------------------------------------------

### `AppenderConsole$new()`

Create a new AppenderConsole.

#### Usage

    AppenderConsole$new(
      threshold = NA_integer_,
      layout = LayoutFormat$new(fmt = "%L [%t] %m %f", timestamp_fmt = "%H:%M:%OS3",
        colors = getOption("lgr.colors", list())),
      filters = NULL,
      connection = NULL
    )

#### Arguments

- `threshold`:

  `character` or `integer` scalar. The minimum log level that triggers
  this Appender. See
  [log_levels](https://s-fleck.github.io/lgr/reference/get_log_levels.md).

- `layout`:

  A [Layout](https://s-fleck.github.io/lgr/reference/Layout.md) that
  determines how
  [LogEvents](https://s-fleck.github.io/lgr/reference/LogEvent.md) are
  formatted before being output by this Appender.

- `filters`:

  A `list` of
  [EventFilters](https://s-fleck.github.io/lgr/reference/EventFilter.md)
  or predicate functions used to filter events for this Appender. See
  [Filterable](https://s-fleck.github.io/lgr/reference/Filterable.md).

- `connection`:

  A connection or a `character` scalar. See the `file` argument of
  [`cat()`](https://rdrr.io/r/base/cat.html) for more details. Defaults
  to [`stdout()`](https://rdrr.io/r/base/showConnections.html), except
  inside knitr rendering processes where it defaults to
  [`stderr()`](https://rdrr.io/r/base/showConnections.html).

------------------------------------------------------------------------

### `AppenderConsole$append()`

Process a
[LogEvent](https://s-fleck.github.io/lgr/reference/LogEvent.md) `event`
by writing it to `$connection`. This method is usually not called by the
user, but invoked by a
[Logger](https://s-fleck.github.io/lgr/reference/Logger.md).

#### Usage

    AppenderConsole$append(event)

#### Arguments

- `event`:

  a [LogEvent](https://s-fleck.github.io/lgr/reference/LogEvent.md)

------------------------------------------------------------------------

### `AppenderConsole$set_connection()`

Set the connection that this Appender writes to.

#### Usage

    AppenderConsole$set_connection(connection)

#### Arguments

- `connection`:

  A connection or a `character` scalar. See the `file` argument of
  [`cat()`](https://rdrr.io/r/base/cat.html) for more details.

## Examples

``` r
# create a new logger with propagate = FALSE to prevent routing to the root
# logger. Please look at the section "Logger Hirarchies" in the package
# vignette for more info.
lg  <- get_logger("test")$set_propagate(FALSE)

lg$add_appender(AppenderConsole$new())
lg$add_appender(AppenderConsole$new(
  layout = LayoutFormat$new("[%t] %c(): [%n] %m", colors = getOption("lgr.colors"))))

# Will output the message twice because we attached two console appenders
lg$warn("A test message")
#> WARN  [14:17:44.594] A test message
#> [2026-07-21 14:17:44.594] eval(): [300] A test message
lg$config(NULL) # reset config
```
