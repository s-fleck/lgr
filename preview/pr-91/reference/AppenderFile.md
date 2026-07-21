# Log to a file

A simple Appender that outputs to a file in the file system. If you plan
to log to text files, consider logging to JSON files and take a look at
AppenderJson, which is a shortcut for `AppenderFile` preconfigured with
[`LayoutJson`](https://s-fleck.github.io/lgr/reference/LayoutJson.md).

## See also

[LayoutFormat](https://s-fleck.github.io/lgr/reference/LayoutFormat.md),
[LayoutJson](https://s-fleck.github.io/lgr/reference/LayoutJson.md)

Other Appenders:
[`Appender`](https://s-fleck.github.io/lgr/reference/Appender.md),
[`AppenderBuffer`](https://s-fleck.github.io/lgr/reference/AppenderBuffer.md),
[`AppenderConsole`](https://s-fleck.github.io/lgr/reference/AppenderConsole.md),
[`AppenderFileRotating`](https://s-fleck.github.io/lgr/reference/AppenderFileRotating.md),
[`AppenderFileRotatingDate`](https://s-fleck.github.io/lgr/reference/AppenderFileRotatingDate.md),
[`AppenderFileRotatingTime`](https://s-fleck.github.io/lgr/reference/AppenderFileRotatingTime.md),
[`AppenderTable`](https://s-fleck.github.io/lgr/reference/AppenderTable.md)

## Super classes

[`Filterable`](https://s-fleck.github.io/lgr/reference/Filterable.md)
-\> [`Appender`](https://s-fleck.github.io/lgr/reference/Appender.md)
-\> `AppenderFile`

## Active bindings

- `file`:

  `character` scalar. path to the log file

- `data`:

  `data.frame`. Contents of `file` parsed to a `data.frame` if used with
  a [Layout](https://s-fleck.github.io/lgr/reference/Layout.md) that
  supports parsing of log file data (notably
  [LayoutJson](https://s-fleck.github.io/lgr/reference/LayoutJson.md)).
  Will throw an error if `Layout` does not support parsing.

- `dt`:

  Like `$data`, but returns a `data.table` instead (requires the
  **data.table** package).

- `destination`:

  The output destination of the `Appender` in human-readable form. This
  is mainly used when printing information about the Appender itself.

## Methods

### Public methods

- [`AppenderFile$new()`](#method-AppenderFile-initialize)

- [`AppenderFile$append()`](#method-AppenderFile-append)

- [`AppenderFile$set_file()`](#method-AppenderFile-set_file)

- [`AppenderFile$show()`](#method-AppenderFile-show)

Inherited methods

- [`Filterable$add_filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-add_filter)
- [`Filterable$filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-filter)
- [`Filterable$remove_filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-remove_filter)
- [`Filterable$set_filters()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-set_filters)
- [`Appender$format()`](https://s-fleck.github.io/lgr/reference/Appender.html#method-format)
- [`Appender$set_layout()`](https://s-fleck.github.io/lgr/reference/Appender.html#method-set_layout)
- [`Appender$set_threshold()`](https://s-fleck.github.io/lgr/reference/Appender.html#method-set_threshold)

------------------------------------------------------------------------

### `AppenderFile$new()`

Create a new AppenderFile.

#### Usage

    AppenderFile$new(
      file,
      threshold = NA_integer_,
      layout = LayoutFormat$new(),
      filters = NULL
    )

#### Arguments

- `file`:

  `character` scalar. Path to the log file. If `file` does not exist it
  will be created.

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

------------------------------------------------------------------------

### `AppenderFile$append()`

Process a
[LogEvent](https://s-fleck.github.io/lgr/reference/LogEvent.md) `event`
by appending it to `$file`. This method is usually not called by the
user, but invoked by a
[Logger](https://s-fleck.github.io/lgr/reference/Logger.md).

#### Usage

    AppenderFile$append(event)

#### Arguments

- `event`:

  a [LogEvent](https://s-fleck.github.io/lgr/reference/LogEvent.md)

------------------------------------------------------------------------

### `AppenderFile$set_file()`

Set a log file

#### Usage

    AppenderFile$set_file(file)

#### Arguments

- `file`:

  `character` scalar. Path to the log file. If `file` does not exist it
  will be created.

------------------------------------------------------------------------

### `AppenderFile$show()`

Display the contents of the log file.

#### Usage

    AppenderFile$show(threshold = NA_integer_, n = 20L)

#### Arguments

- `threshold`:

  `character` or `integer` scalar. The minimum log level that should be
  displayed.

- `n`:

  `integer` scalar. Show only the last `n` log entries that match
  `threshold`.

## Super classes

[`Filterable`](https://s-fleck.github.io/lgr/reference/Filterable.md)
-\> [`Appender`](https://s-fleck.github.io/lgr/reference/Appender.md)
-\> `AppenderFile` -\> `AppenderJson`

## Methods

### Public methods

- [`AppenderJson$new()`](#method-AppenderJson-initialize)

Inherited methods

- [`Filterable$add_filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-add_filter)
- [`Filterable$filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-filter)
- [`Filterable$remove_filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-remove_filter)
- [`Filterable$set_filters()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-set_filters)
- [`Appender$format()`](https://s-fleck.github.io/lgr/reference/Appender.html#method-format)
- [`Appender$set_layout()`](https://s-fleck.github.io/lgr/reference/Appender.html#method-set_layout)
- [`Appender$set_threshold()`](https://s-fleck.github.io/lgr/reference/Appender.html#method-set_threshold)
- [`AppenderFile$append()`](https://s-fleck.github.io/lgr/reference/AppenderFile.html#method-append)
- [`AppenderFile$set_file()`](https://s-fleck.github.io/lgr/reference/AppenderFile.html#method-set_file)
- [`AppenderFile$show()`](https://s-fleck.github.io/lgr/reference/AppenderFile.html#method-show)

------------------------------------------------------------------------

### `AppenderJson$new()`

Create a new AppenderJson.

#### Usage

    AppenderJson$new(
      file,
      threshold = NA_integer_,
      layout = LayoutJson$new(),
      filters = NULL
    )

#### Arguments

- `file`:

  `character` scalar. Path to the log file. If `file` does not exist it
  will be created.

- `threshold`:

  `character` or `integer` scalar. The minimum log level that triggers
  this Appender. See
  [log_levels](https://s-fleck.github.io/lgr/reference/get_log_levels.md).

- `layout`:

  A [Layout](https://s-fleck.github.io/lgr/reference/Layout.md) that
  determines how
  [LogEvents](https://s-fleck.github.io/lgr/reference/LogEvent.md) are
  formatted before being output by this Appender. Defaults to
  [LayoutJson](https://s-fleck.github.io/lgr/reference/LayoutJson.md).

- `filters`:

  A `list` of
  [EventFilters](https://s-fleck.github.io/lgr/reference/EventFilter.md)
  or predicate functions used to filter events for this Appender. See
  [Filterable](https://s-fleck.github.io/lgr/reference/Filterable.md).

## Examples

``` r
lg <- get_logger("test")
default <- tempfile()
fancy <- tempfile()
json <- tempfile()

lg$add_appender(AppenderFile$new(default), "default")
lg$add_appender(
  AppenderFile$new(fancy, layout = LayoutFormat$new("[%t] %c(): %L %m")), "fancy"
)
lg$add_appender(
  AppenderFile$new(json, layout = LayoutJson$new()), "json"
)

lg$info("A test message")
#> INFO  [14:17:44.804] A test message

readLines(default)
#> [1] "INFO  [2026-07-21 14:17:44.804] A test message"
readLines(fancy)
#> [1] "[2026-07-21 14:17:44.804] eval(): INFO  A test message"
readLines(json)
#> [1] "{\"level\":400,\"timestamp\":\"2026-07-21 14:17:44\",\"logger\":\"test\",\"caller\":\"eval\",\"msg\":\"A test message\"}"

# cleanup
lg$config(NULL)
unlink(default)
unlink(fancy)
unlink(json)
tf <- tempfile()
lg <- get_logger("test")$
  set_appenders(AppenderJson$new(tf))$
  set_propagate(FALSE)

lg$info("A test message")
lg$info("A test message %s strings", "with format strings", and = "custom_fields")

lg$appenders[[1]]$show()
#> {"level":400,"timestamp":"2026-07-21 14:17:44","logger":"test","caller":"eval","msg":"A test message"}
#> {"level":400,"timestamp":"2026-07-21 14:17:44","logger":"test","caller":"eval","msg":"A test message with format strings strings","and":"custom_fields"}
lg$appenders[[1]]$data
#>   level           timestamp logger caller
#> 1   400 2026-07-21 14:17:44   test   eval
#> 2   400 2026-07-21 14:17:44   test   eval
#>                                          msg           and
#> 1                             A test message          <NA>
#> 2 A test message with format strings strings custom_fields

# cleanup
lg$config(NULL)
unlink(tf)
```
