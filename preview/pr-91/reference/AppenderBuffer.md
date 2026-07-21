# Log to a memory buffer

An Appender that Buffers LogEvents in-memory and and redirects them to
other Appenders once certain conditions are met.

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

[`Filterable`](https://s-fleck.github.io/lgr/reference/Filterable.md)
-\> [`Appender`](https://s-fleck.github.io/lgr/reference/Appender.md)
-\>
[`AppenderMemory`](https://s-fleck.github.io/lgr/reference/AppenderMemory.md)
-\> `AppenderBuffer`

## Active bindings

- `appenders`:

  A `list` of
  [Appenders](https://s-fleck.github.io/lgr/reference/Appender.md). Like
  for a [Logger](https://s-fleck.github.io/lgr/reference/Logger.md),
  buffered events will be passed on to these Appenders once a flush is
  triggered. See `$set_appenders()`.

- `destination`:

  The output destination of the `Appender` in human-readable form. This
  is mainly used when printing information about the Appender itself.

## Methods

### Public methods

- [`AppenderBuffer$new()`](#method-AppenderBuffer-initialize)

- [`AppenderBuffer$flush()`](#method-AppenderBuffer-flush)

- [`AppenderBuffer$clear()`](#method-AppenderBuffer-clear)

- [`AppenderBuffer$set_appenders()`](#method-AppenderBuffer-set_appenders)

- [`AppenderBuffer$add_appender()`](#method-AppenderBuffer-add_appender)

- [`AppenderBuffer$remove_appender()`](#method-AppenderBuffer-remove_appender)

- [`AppenderBuffer$format()`](#method-AppenderBuffer-format)

Inherited methods

- [`Filterable$add_filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-add_filter)
- [`Filterable$filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-filter)
- [`Filterable$remove_filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-remove_filter)
- [`Filterable$set_filters()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-set_filters)
- [`Appender$set_layout()`](https://s-fleck.github.io/lgr/reference/Appender.html#method-set_layout)
- [`Appender$set_threshold()`](https://s-fleck.github.io/lgr/reference/Appender.html#method-set_threshold)
- [`AppenderMemory$append()`](https://s-fleck.github.io/lgr/reference/AppenderMemory.html#method-append)
- [`AppenderMemory$set_buffer_size()`](https://s-fleck.github.io/lgr/reference/AppenderMemory.html#method-set_buffer_size)
- [`AppenderMemory$set_flush_on_exit()`](https://s-fleck.github.io/lgr/reference/AppenderMemory.html#method-set_flush_on_exit)
- [`AppenderMemory$set_flush_on_rotate()`](https://s-fleck.github.io/lgr/reference/AppenderMemory.html#method-set_flush_on_rotate)
- [`AppenderMemory$set_flush_threshold()`](https://s-fleck.github.io/lgr/reference/AppenderMemory.html#method-set_flush_threshold)
- [`AppenderMemory$set_should_flush()`](https://s-fleck.github.io/lgr/reference/AppenderMemory.html#method-set_should_flush)
- [`AppenderMemory$show()`](https://s-fleck.github.io/lgr/reference/AppenderMemory.html#method-show)

------------------------------------------------------------------------

### `AppenderBuffer$new()`

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

- `appenders`:

  single [Appender](https://s-fleck.github.io/lgr/reference/Appender.md)
  or a `list` thereof to dispatch the buffered events to when the buffer
  is flushed. See `$set_appenders()`.

- `buffer_size`:

  an `integer` scalar `>= 0`. The maximum number of
  [LogEvents](https://s-fleck.github.io/lgr/reference/LogEvent.md) to
  buffer. See `$set_buffer_size()`.

- `flush_threshold`:

  A `numeric` or `character`
  [threshold](https://s-fleck.github.io/lgr/reference/get_log_levels.md)
  that triggers flushing the buffer. See `$set_flush_threshold()`.

- `flush_on_exit`:

  A `logical` scalar. Should the buffer be flushed when the Appender is
  destroyed? See `$set_flush_on_exit()`.

- `flush_on_rotate`:

  A `logical` scalar. Should the buffer be flushed if `buffer_size` is
  exceeded? See `$set_flush_on_rotate()`.

- `should_flush`:

  A `function` with the single argument `event` that can trigger
  flushing the buffer, or `NULL`. See `$set_should_flush()`.

- `filters`:

  A `list` of
  [EventFilters](https://s-fleck.github.io/lgr/reference/EventFilter.md)
  or predicate functions used to filter events for this Appender. See
  [Filterable](https://s-fleck.github.io/lgr/reference/Filterable.md).

------------------------------------------------------------------------

### `AppenderBuffer$flush()`

Sends the buffer's contents to all attached Appenders and then clears
the Buffer

#### Usage

    AppenderBuffer$flush()

------------------------------------------------------------------------

### `AppenderBuffer$clear()`

Clears the buffer, discarding all buffered Events

#### Usage

    AppenderBuffer$clear()

------------------------------------------------------------------------

### `AppenderBuffer$set_appenders()`

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

### `AppenderBuffer$add_appender()`

Add an [Appender](https://s-fleck.github.io/lgr/reference/Appender.md)
to the AppenderBuffer. Supplying a `name` is optional but recommended.
After adding an Appender with
`appender$add_appender(AppenderConsole$new(), name = "console")` you can
refer to it via `appender$appenders$console`. `$remove_appender()` can
remove an Appender by position or name.

#### Usage

    AppenderBuffer$add_appender(appender, name = NULL)

#### Arguments

- `appender`:

  a single
  [Appender](https://s-fleck.github.io/lgr/reference/Appender.md)

- `name`:

  a `character` scalar. Optional but recommended.

------------------------------------------------------------------------

### `AppenderBuffer$remove_appender()`

remove an appender

#### Usage

    AppenderBuffer$remove_appender(pos)

#### Arguments

- `pos`:

  `integer` index or `character` name of the Appender(s) to remove

------------------------------------------------------------------------

### `AppenderBuffer$format()`

Format the Appender for printing (see also
[`print.Appender()`](https://s-fleck.github.io/lgr/reference/print.Appender.md)).

#### Usage

    AppenderBuffer$format(...)

#### Arguments

- `...`:

  ignored
