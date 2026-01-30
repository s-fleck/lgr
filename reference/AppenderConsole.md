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

[`lgr::Filterable`](https://s-fleck.github.io/lgr/reference/Filterable.md)
-\>
[`lgr::Appender`](https://s-fleck.github.io/lgr/reference/Appender.md)
-\> `AppenderConsole`

## Methods

### Public methods

- [`AppenderConsole$new()`](#method-AppenderConsole-new)

- [`AppenderConsole$append()`](#method-AppenderConsole-append)

- [`AppenderConsole$set_connection()`](#method-AppenderConsole-set_connection)

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

    AppenderConsole$new(
      threshold = NA_integer_,
      layout = LayoutFormat$new(fmt = "%L [%t] %m %f", timestamp_fmt = "%H:%M:%OS3",
        colors = getOption("lgr.colors", list())),
      filters = NULL,
      connection = NULL
    )

#### Arguments

- `connection`:

  A connection or a `character` scalar. See the `file` argument of
  [`cat()`](https://rdrr.io/r/base/cat.html) for more details. Defaults
  to [`stdout()`](https://rdrr.io/r/base/showConnections.html), except
  inside knitr rendering processes where it defaults to
  [`stderr()`](https://rdrr.io/r/base/showConnections.html).

------------------------------------------------------------------------

### Method [`append()`](https://rdrr.io/r/base/append.html)

#### Usage

    AppenderConsole$append(event)

------------------------------------------------------------------------

### Method `set_connection()`

#### Usage

    AppenderConsole$set_connection(connection)

#### Arguments

- `connection`:

  A connection or a `character` scalar. See the `file` argument of
  [`cat()`](https://rdrr.io/r/base/cat.html) for more details. Defaults
  to [`stdout()`](https://rdrr.io/r/base/showConnections.html), except
  inside knitr rendering processes where it defaults to
  [`stderr()`](https://rdrr.io/r/base/showConnections.html).

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
#> WARN  [13:41:45.536] A test message
#> [2026-01-30 13:41:45.536] eval(): [300] A test message
lg$config(NULL) # reset config
#> <Logger> [info] test
#> 
#> inherited appenders:
#>   console: <AppenderConsole> [all] -> console
```
