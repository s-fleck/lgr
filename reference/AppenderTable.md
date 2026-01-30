# Abstract class for logging to tabular structures

**NOTE**: This is an *abstract class*. Abstract classes cannot be
instantiated directly, but are exported for package developers that want
to extend lgr - for example by creating their own
[Appenders](https://s-fleck.github.io/lgr/reference/Appender.md) or
[Layouts](https://s-fleck.github.io/lgr/reference/Layout.md). Please
refer to the *see also* section for actual implementations of this
class.

AppenderTable is extended by Appenders that write to a data source that
can be interpreted as tables, (usually a `data.frame`). Examples are
`AppenderDbi`, `AppenderRjdbc` and `AppenderDt` from the
[lgrExtra](https://github.com/s-fleck/lgrExtra) package.

## See also

Other abstract classes:
[`Appender`](https://s-fleck.github.io/lgr/reference/Appender.md),
[`AppenderMemory`](https://s-fleck.github.io/lgr/reference/AppenderMemory.md),
[`Filterable`](https://s-fleck.github.io/lgr/reference/Filterable.md)

Other Appenders:
[`Appender`](https://s-fleck.github.io/lgr/reference/Appender.md),
[`AppenderBuffer`](https://s-fleck.github.io/lgr/reference/AppenderBuffer.md),
[`AppenderConsole`](https://s-fleck.github.io/lgr/reference/AppenderConsole.md),
[`AppenderFile`](https://s-fleck.github.io/lgr/reference/AppenderFile.md),
[`AppenderFileRotating`](https://s-fleck.github.io/lgr/reference/AppenderFileRotating.md),
[`AppenderFileRotatingDate`](https://s-fleck.github.io/lgr/reference/AppenderFileRotatingDate.md),
[`AppenderFileRotatingTime`](https://s-fleck.github.io/lgr/reference/AppenderFileRotatingTime.md)

## Super classes

[`lgr::Filterable`](https://s-fleck.github.io/lgr/reference/Filterable.md)
-\>
[`lgr::Appender`](https://s-fleck.github.io/lgr/reference/Appender.md)
-\> `AppenderTable`

## Active bindings

- `data`:

  `character` scalar. Contents of the table, parsed to a `data.frame`.

- `data`:

  `character` scalar. Like `$data`, but returns a `data.table` instead
  (requires the **data.table** package).

## Methods

### Public methods

- [`AppenderTable$new()`](#method-AppenderTable-new)

- [`AppenderTable$show()`](#method-AppenderTable-show)

- [`AppenderTable$format()`](#method-AppenderTable-format)

Inherited methods

- [`lgr::Filterable$add_filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-add_filter)
- [`lgr::Filterable$filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-filter)
- [`lgr::Filterable$remove_filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-remove_filter)
- [`lgr::Filterable$set_filters()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-set_filters)
- [`lgr::Appender$append()`](https://s-fleck.github.io/lgr/reference/Appender.html#method-append)
- [`lgr::Appender$set_layout()`](https://s-fleck.github.io/lgr/reference/Appender.html#method-set_layout)
- [`lgr::Appender$set_threshold()`](https://s-fleck.github.io/lgr/reference/Appender.html#method-set_threshold)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    AppenderTable$new(...)

------------------------------------------------------------------------

### Method `show()`

Show recent log entries

#### Usage

    AppenderTable$show(threshold = NA_integer_, n = 20L)

#### Arguments

- `threshold`:

  an `integer` or `character`
  [threshold](https://s-fleck.github.io/lgr/reference/get_log_levels.md).
  Only show events with a log level at or below this threshold.

- `n`:

  a positive `integer` scalar. Show at most that many entries

------------------------------------------------------------------------

### Method [`format()`](https://rdrr.io/r/base/format.html)

#### Usage

    AppenderTable$format(color = FALSE, ...)
