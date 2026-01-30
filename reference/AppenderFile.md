# Log to a file

A simple Appender that outputs to a file in the file system. If you plan
to log to text files, consider logging to JSON files and take a look at
AppenderJson, which is a shortcut for `AppenderFile` preconfigured with
[`LayoutJson`](https://s-fleck.github.io/lgr/reference/LayoutJson.md).

## See also

[LayoutFormat](https://s-fleck.github.io/lgr/reference/LayoutFormat.md),
[LayoutJson](https://s-fleck.github.io/lgr/reference/LayoutJson.md)

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

Other Appenders:
[`Appender`](https://s-fleck.github.io/lgr/reference/Appender.md),
[`AppenderBuffer`](https://s-fleck.github.io/lgr/reference/AppenderBuffer.md),
[`AppenderConsole`](https://s-fleck.github.io/lgr/reference/AppenderConsole.md),
[`AppenderFileRotating`](https://s-fleck.github.io/lgr/reference/AppenderFileRotating.md),
[`AppenderFileRotatingDate`](https://s-fleck.github.io/lgr/reference/AppenderFileRotatingDate.md),
[`AppenderFileRotatingTime`](https://s-fleck.github.io/lgr/reference/AppenderFileRotatingTime.md),
[`AppenderTable`](https://s-fleck.github.io/lgr/reference/AppenderTable.md)

## Super classes

[`lgr::Filterable`](https://s-fleck.github.io/lgr/reference/Filterable.md)
-\>
[`lgr::Appender`](https://s-fleck.github.io/lgr/reference/Appender.md)
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

- `data`:

  `character` scalar. Like `$data`, but returns a `data.table` instead
  (requires the **data.table** package).

## Methods

### Public methods

- [`AppenderFile$new()`](#method-AppenderFile-new)

- [`AppenderFile$append()`](#method-AppenderFile-append)

- [`AppenderFile$set_file()`](#method-AppenderFile-set_file)

- [`AppenderFile$show()`](#method-AppenderFile-show)

Inherited methods

- [`lgr::Filterable$add_filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-add_filter)
- [`lgr::Filterable$filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-filter)
- [`lgr::Filterable$remove_filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-remove_filter)
- [`lgr::Filterable$set_filters()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-set_filters)
- [`lgr::Appender$format()`](https://s-fleck.github.io/lgr/reference/Appender.html#method-format)
- [`lgr::Appender$set_layout()`](https://s-fleck.github.io/lgr/reference/Appender.html#method-set_layout)
- [`lgr::Appender$set_threshold()`](https://s-fleck.github.io/lgr/reference/Appender.html#method-set_threshold)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    AppenderFile$new(
      file,
      threshold = NA_integer_,
      layout = LayoutFormat$new(),
      filters = NULL
    )

------------------------------------------------------------------------

### Method [`append()`](https://rdrr.io/r/base/append.html)

#### Usage

    AppenderFile$append(event)

------------------------------------------------------------------------

### Method `set_file()`

Set a log file

#### Usage

    AppenderFile$set_file(file)

#### Arguments

- `file`:

  `character` scalar. Path to the log file. If `file` does not exist it
  will be created.

------------------------------------------------------------------------

### Method `show()`

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

[`lgr::Filterable`](https://s-fleck.github.io/lgr/reference/Filterable.md)
-\>
[`lgr::Appender`](https://s-fleck.github.io/lgr/reference/Appender.md)
-\> `lgr::AppenderFile` -\> `AppenderJson`

## Methods

### Public methods

- [`AppenderJson$new()`](#method-AppenderJson-new)

Inherited methods

- [`lgr::Filterable$add_filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-add_filter)
- [`lgr::Filterable$filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-filter)
- [`lgr::Filterable$remove_filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-remove_filter)
- [`lgr::Filterable$set_filters()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-set_filters)
- [`lgr::Appender$format()`](https://s-fleck.github.io/lgr/reference/Appender.html#method-format)
- [`lgr::Appender$set_layout()`](https://s-fleck.github.io/lgr/reference/Appender.html#method-set_layout)
- [`lgr::Appender$set_threshold()`](https://s-fleck.github.io/lgr/reference/Appender.html#method-set_threshold)
- [`lgr::AppenderFile$append()`](https://s-fleck.github.io/lgr/reference/AppenderFile.html#method-append)
- [`lgr::AppenderFile$set_file()`](https://s-fleck.github.io/lgr/reference/AppenderFile.html#method-set_file)
- [`lgr::AppenderFile$show()`](https://s-fleck.github.io/lgr/reference/AppenderFile.html#method-show)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    AppenderJson$new(
      file,
      threshold = NA_integer_,
      layout = LayoutJson$new(),
      filters = NULL
    )

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
#> INFO  [11:47:27.731] A test message

readLines(default)
#> [1] "INFO  [2026-01-30 11:47:27.731] A test message"
readLines(fancy)
#> [1] "[2026-01-30 11:47:27.731] eval(): INFO  A test message"
readLines(json)
#> [1] "{\"level\":400,\"timestamp\":\"2026-01-30 11:47:27\",\"logger\":\"test\",\"caller\":\"eval\",\"msg\":\"A test message\"}"

# cleanup
lg$config(NULL)
#> <Logger> [info] test
#> 
#> inherited appenders:
#>   console: <AppenderConsole> [all] -> console
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
#> {"level":400,"timestamp":"2026-01-30 11:47:27","logger":"test","caller":"eval","msg":"A test message"}
#> {"level":400,"timestamp":"2026-01-30 11:47:27","logger":"test","caller":"eval","msg":"A test message with format strings strings","and":"custom_fields"}
lg$appenders[[1]]$data
#>   level           timestamp logger caller
#> 1   400 2026-01-30 11:47:27   test   eval
#> 2   400 2026-01-30 11:47:27   test   eval
#>                                          msg           and
#> 1                             A test message          <NA>
#> 2 A test message with format strings strings custom_fields

# cleanup
lg$config(NULL)
#> <Logger> [info] test
#> 
#> inherited appenders:
#>   console: <AppenderConsole> [all] -> console
unlink(tf)
```
