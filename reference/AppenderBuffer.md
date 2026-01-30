# Log to a memory buffer

An Appender that Buffers LogEvents in-memory and and redirects them to
other Appenders once certain conditions are met.

## Fields

- `appenders`, `set_appenders()`:

  Like for a
  [Logger](https://s-fleck.github.io/lgr/reference/Logger.md). Buffered
  events will be passed on to these Appenders once a flush is triggered

- `flush_on_exit, set_flush_on_exit(x)`:

  `TRUE` or `FALSE`: Whether the buffer should be flushed when the
  Appender is garbage collected (f.e when you close R)

- `flush_on_rotate, set_flush_on_rotate`:

  `TRUE` or `FALSE`: Whether the buffer should be flushed when the
  Buffer is full (f.e when you close R). Setting this to off can have
  slightly negative performance impacts.

## See also

[LayoutFormat](https://s-fleck.github.io/lgr/reference/LayoutFormat.md)

Other Appenders:
[`Appender`](https://s-fleck.github.io/lgr/reference/Appender.md),
[`AppenderConsole`](https://s-fleck.github.io/lgr/reference/AppenderConsole.md),
[`AppenderFile`](https://s-fleck.github.io/lgr/reference/AppenderFile.md),
[`AppenderFileRotating`](https://s-fleck.github.io/lgr/reference/AppenderFileRotating.md),
[`AppenderFileRotatingDate`](https://s-fleck.github.io/lgr/reference/AppenderFileRotatingDate.md),
[`AppenderFileRotatingTime`](https://s-fleck.github.io/lgr/reference/AppenderFileRotatingTime.md),
[`AppenderTable`](https://s-fleck.github.io/lgr/reference/AppenderTable.md)

## Super classes

[`lgr::Filterable`](https://s-fleck.github.io/lgr/reference/Filterable.md)
-\>
[`lgr::Appender`](https://s-fleck.github.io/lgr/reference/Appender.md)
-\>
[`lgr::AppenderMemory`](https://s-fleck.github.io/lgr/reference/AppenderMemory.md)
-\> `AppenderBuffer`

## Methods

### Public methods

- [`AppenderBuffer$new()`](#method-AppenderBuffer-new)

- [`AppenderBuffer$flush()`](#method-AppenderBuffer-flush)

- [`AppenderBuffer$clear()`](#method-AppenderBuffer-clear)

- [`AppenderBuffer$set_appenders()`](#method-AppenderBuffer-set_appenders)

- [`AppenderBuffer$add_appender()`](#method-AppenderBuffer-add_appender)

- [`AppenderBuffer$remove_appender()`](#method-AppenderBuffer-remove_appender)

- [`AppenderBuffer$format()`](#method-AppenderBuffer-format)

Inherited methods

- [`lgr::Filterable$add_filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-add_filter)
- [`lgr::Filterable$filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-filter)
- [`lgr::Filterable$remove_filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-remove_filter)
- [`lgr::Filterable$set_filters()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-set_filters)
- [`lgr::Appender$set_layout()`](https://s-fleck.github.io/lgr/reference/Appender.html#method-set_layout)
- [`lgr::Appender$set_threshold()`](https://s-fleck.github.io/lgr/reference/Appender.html#method-set_threshold)
- [`lgr::AppenderMemory$append()`](https://s-fleck.github.io/lgr/reference/AppenderMemory.html#method-append)
- [`lgr::AppenderMemory$set_buffer_size()`](https://s-fleck.github.io/lgr/reference/AppenderMemory.html#method-set_buffer_size)
- [`lgr::AppenderMemory$set_flush_on_exit()`](https://s-fleck.github.io/lgr/reference/AppenderMemory.html#method-set_flush_on_exit)
- [`lgr::AppenderMemory$set_flush_on_rotate()`](https://s-fleck.github.io/lgr/reference/AppenderMemory.html#method-set_flush_on_rotate)
- [`lgr::AppenderMemory$set_flush_threshold()`](https://s-fleck.github.io/lgr/reference/AppenderMemory.html#method-set_flush_threshold)
- [`lgr::AppenderMemory$set_should_flush()`](https://s-fleck.github.io/lgr/reference/AppenderMemory.html#method-set_should_flush)
- [`lgr::AppenderMemory$show()`](https://s-fleck.github.io/lgr/reference/AppenderMemory.html#method-show)

------------------------------------------------------------------------

### Method `new()`

The [Layout](https://s-fleck.github.io/lgr/reference/Layout.md) for this
Appender is used only to format console output of its `$show()` method.

#### Usage

    AppenderBuffer$new(
      threshold = NA_integer_,
      layout = LayoutFormat$new(fmt = "%L [%t] %m", timestamp_fmt = "%H:%M:%S", colors
        = getOption("lgr.colors")),
      appenders = NULL,
      buffer_size = 1000,
      flush_threshold = NULL,
      flush_on_exit = TRUE,
      flush_on_rotate = TRUE,
      should_flush = NULL,
      filters = NULL
    )

------------------------------------------------------------------------

### Method [`flush()`](https://rdrr.io/r/base/connections.html)

Sends the buffer's contents to all attached Appenders and then clears
the Buffer

#### Usage

    AppenderBuffer$flush()

------------------------------------------------------------------------

### Method `clear()`

Clears the buffer, discarding all buffered Events

#### Usage

    AppenderBuffer$clear()

------------------------------------------------------------------------

### Method `set_appenders()`

Exactly like A
[Logger](https://s-fleck.github.io/lgr/reference/Logger.md), an
AppenderBuffer can have an arbitrary amount of Appenders attached. When
the buffer is flushed, the buffered events are dispatched to these
Appenders.

#### Usage

    AppenderBuffer$set_appenders(x)

#### Arguments

- `x`:

  single [Appender](https://s-fleck.github.io/lgr/reference/Appender.md)
  or a `list` thereof. Appenders control the output of a Logger. Be
  aware that a Logger also inherits the Appenders of its ancestors (see
  [`vignette("lgr", package = "lgr")`](https://s-fleck.github.io/lgr/articles/lgr.md)
  for more info about Logger inheritance).

------------------------------------------------------------------------

### Method [`add_appender()`](https://s-fleck.github.io/lgr/reference/simple_logging.md)

Add an Appender to the AppenderBuffer

Add or remove an
[Appender](https://s-fleck.github.io/lgr/reference/Appender.md).
Supplying a `name` is optional but recommended. After adding an Appender
with `appender$add_appender(AppenderConsole$new(), name = "console")`
you can refer to it via `appender$appenders$console`.
[`remove_appender()`](https://s-fleck.github.io/lgr/reference/simple_logging.md)
can remove an Appender by position or name.

#### Usage

    AppenderBuffer$add_appender(appender, name = NULL)

#### Arguments

- `appender`:

  a single
  [Appender](https://s-fleck.github.io/lgr/reference/Appender.md)

- `name`:

  a `character` scalar. Optional but recommended.

------------------------------------------------------------------------

### Method [`remove_appender()`](https://s-fleck.github.io/lgr/reference/simple_logging.md)

remove an appender

#### Usage

    AppenderBuffer$remove_appender(pos)

#### Arguments

- `pos`:

  `integer` index or `character` name of the Appender(s) to remove

------------------------------------------------------------------------

### Method [`format()`](https://rdrr.io/r/base/format.html)

#### Usage

    AppenderBuffer$format(...)
