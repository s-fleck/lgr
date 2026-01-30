# LogEvents - The atomic unit of logging

A `LogEvent` is a single unit of data that should be logged. `LogEvents`
are usually created by a
[Logger](https://s-fleck.github.io/lgr/reference/Logger.md), and then
processed by one more
[Appenders](https://s-fleck.github.io/lgr/reference/Appender.md). They
do not need to be instantiated manually except for testing and
experimentation; however, if you plan on writing your own Appenders or
Layouts you need to understand LogEvents.

## See also

[`as.data.frame.LogEvent()`](https://s-fleck.github.io/lgr/reference/as.data.frame.LogEvent.md)

Other docs relevant for extending lgr:
[`as_LogEvent()`](https://s-fleck.github.io/lgr/reference/as_LogEvent.md),
[`event_list()`](https://s-fleck.github.io/lgr/reference/event_list.md),
[`standardize_threshold()`](https://s-fleck.github.io/lgr/reference/standardize_threshold.md)

## Public fields

- `level`:

  `integer`. The
  [log_level](https://s-fleck.github.io/lgr/reference/get_log_levels.md)
  / priority of the LogEvent. Use the active binding `level_name` to get
  the `character` representation instead.

- `timestamp`:

  [`POSIXct`](https://rdrr.io/r/base/DateTimeClasses.html). The time
  when then the LogEvent was created.

- `caller`:

  `character`. The name of the calling function.

- `msg`:

  `character`. The log message.

- `.logger`:

  [Logger](https://s-fleck.github.io/lgr/reference/Logger.md). A
  reference to the Logger that created the event (equivalent to
  `get_logger(event$logger)`).

- `rawMsg`:

  `character`. The raw log message without string interpolation.

## Active bindings

- `values`:

  `list`. All values stored in the `LogEvent`, including all *custom
  fields*, but not including `event$.logger`.

- `level_name`:

  `character`. The
  [log_level](https://s-fleck.github.io/lgr/reference/get_log_levels.md)
  / priority of the LogEvent labelled according to
  `getOption("lgr.log_levels")`

- `logger`:

  `character` scalar. The name of the Logger that created this event,
  equivalent to `event$.logger$name`)

## Methods

### Public methods

- [`LogEvent$new()`](#method-LogEvent-new)

- [`LogEvent$clone()`](#method-LogEvent-clone)

------------------------------------------------------------------------

### Method `new()`

The arguments to `LogEvent$new()` directly translate to the fields
stored in the `LogEvent`. Usually these values will be scalars, but
(except for `"logger"`) they can also be vectors if they are all of the
same length (or scalars that will be recycled). In this case the event
will be treated by the
[Appenders](https://s-fleck.github.io/lgr/reference/Appender.md) and
[Layouts](https://s-fleck.github.io/lgr/reference/Layout.md) as if
several separate events.

#### Usage

    LogEvent$new(
      logger,
      level = 400,
      timestamp = Sys.time(),
      caller = NA,
      msg = NA,
      rawMsg = msg,
      ...
    )

#### Arguments

- `logger, level, timestamp, caller, msg`:

  see **Public fields**.

- `...`:

  All named arguments in `...` will be added to the LogEvent as **custom
  fields**. You can store arbitrary R objects in LogEvents this way, but
  not all Appenders will support them. See
  [AppenderJson](https://s-fleck.github.io/lgr/reference/AppenderFile.md)
  for

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LogEvent$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
lg <- get_logger("test")
lg$error("foo bar")
#> ERROR [11:47:31.761] foo bar

# The last LogEvent produced by a Logger is stored in its `last_event` field
lg$last_event  # formatted console output
#> ERROR [2026-01-30 11:47:31] foo bar
lg$last_event$values  # values stored in the event
#> $level
#> [1] 200
#> 
#> $timestamp
#> [1] "2026-01-30 11:47:31 UTC"
#> 
#> $logger
#> [1] "test"
#> 
#> $caller
#> [1] "eval"
#> 
#> $msg
#> [1] "foo bar"
#> 
#> $rawMsg
#> [1] "foo bar"
#> 

# Also contains the Logger that created it as .logger
lg$last_event$logger
#> [1] "test"
# equivalent to
lg$last_event$.logger$name
#> [1] "test"

# This is really a reference to the complete Logger, so the following is
# possible (though nonsensical)
lg$last_event$.logger$last_event$msg
#> [1] "foo bar"
identical(lg, lg$last_event$.logger)
#> [1] TRUE
lg$config(NULL)  # reset logger config
#> <Logger> [info] test
#> 
#> inherited appenders:
#>   console: <AppenderConsole> [all] -> console
```
