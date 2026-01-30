# Abstract class for logging to memory buffers

**NOTE**: This is an *abstract class*. Abstract classes cannot be
instantiated directly, but are exported for package developers that want
to extend lgr - for example by creating their own
[Appenders](https://s-fleck.github.io/lgr/reference/Appender.md) or
[Layouts](https://s-fleck.github.io/lgr/reference/Layout.md). Please
refer to the *see also* section for actual implementations of this
class.

AppenderMemory is extended by Appenders that retain an in-memory event
buffer, such as
[AppenderBuffer](https://s-fleck.github.io/lgr/reference/AppenderBuffer.md)
and `AppenderPushbullet` from the
[lgrExtra](https://github.com/s-fleck/lgrExtra) package.

## See also

[LayoutFormat](https://s-fleck.github.io/lgr/reference/LayoutFormat.md)

Other abstract classes:
[`Appender`](https://s-fleck.github.io/lgr/reference/Appender.md),
[`AppenderTable`](https://s-fleck.github.io/lgr/reference/AppenderTable.md),
[`Filterable`](https://s-fleck.github.io/lgr/reference/Filterable.md)

## Super classes

[`lgr::Filterable`](https://s-fleck.github.io/lgr/reference/Filterable.md)
-\>
[`lgr::Appender`](https://s-fleck.github.io/lgr/reference/Appender.md)
-\> `AppenderMemory`

## Active bindings

- `flush_on_exit`:

  A `logical` scalar. Should the buffer be flushed if the Appender is
  destroyed (e.g. because the R session is terminated)?

- `flush_on_rotate`:

  A `logical` scalar. Should the buffer be flushed when it is rotated
  because `$buffer_size` is exceeded?

- `should_flush`:

  A `function` with exactly one arguments: `event`. `$append()` calls
  this function internally on the current
  [LogEvent](https://s-fleck.github.io/lgr/reference/LogEvent.md) and
  flushes the buffer if it evaluates to `TRUE`.

- `buffer_size`:

  `integer` scalar `>= 0`. Maximum number of
  [LogEvents](https://s-fleck.github.io/lgr/reference/LogEvent.md) to
  buffer.

- `flush_threshold`:

  A `numeric` or `character` threshold.
  [LogEvents](https://s-fleck.github.io/lgr/reference/LogEvent.md) with
  a
  [log_level](https://s-fleck.github.io/lgr/reference/get_log_levels.md)
  equal to or lower than this threshold trigger flushing the buffer.

- `buffer_events`:

  A `list` of
  [LogEvents](https://s-fleck.github.io/lgr/reference/LogEvent.md).
  Contents of the buffer.

- `buffer_events`:

  A `data.frame`. Contents of the buffer converted to a `data.frame`.

- `buffer_events`:

  A `data.frame`. Contents of the buffer converted to a `data.table`.

## Methods

### Public methods

- [`AppenderMemory$new()`](#method-AppenderMemory-new)

- [`AppenderMemory$append()`](#method-AppenderMemory-append)

- [`AppenderMemory$flush()`](#method-AppenderMemory-flush)

- [`AppenderMemory$clear()`](#method-AppenderMemory-clear)

- [`AppenderMemory$set_buffer_size()`](#method-AppenderMemory-set_buffer_size)

- [`AppenderMemory$set_should_flush()`](#method-AppenderMemory-set_should_flush)

- [`AppenderMemory$set_flush_on_exit()`](#method-AppenderMemory-set_flush_on_exit)

- [`AppenderMemory$set_flush_on_rotate()`](#method-AppenderMemory-set_flush_on_rotate)

- [`AppenderMemory$set_flush_threshold()`](#method-AppenderMemory-set_flush_threshold)

- [`AppenderMemory$show()`](#method-AppenderMemory-show)

- [`AppenderMemory$format()`](#method-AppenderMemory-format)

Inherited methods

- [`lgr::Filterable$add_filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-add_filter)
- [`lgr::Filterable$filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-filter)
- [`lgr::Filterable$remove_filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-remove_filter)
- [`lgr::Filterable$set_filters()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-set_filters)
- [`lgr::Appender$set_layout()`](https://s-fleck.github.io/lgr/reference/Appender.html#method-set_layout)
- [`lgr::Appender$set_threshold()`](https://s-fleck.github.io/lgr/reference/Appender.html#method-set_threshold)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    AppenderMemory$new(...)

------------------------------------------------------------------------

### Method [`append()`](https://rdrr.io/r/base/append.html)

#### Usage

    AppenderMemory$append(event)

------------------------------------------------------------------------

### Method [`flush()`](https://rdrr.io/r/base/connections.html)

Sends the buffer's contents to all attached Appenders and then clears
the Buffer

#### Usage

    AppenderMemory$flush()

------------------------------------------------------------------------

### Method `clear()`

Clears the buffer, discarding all buffered Events

#### Usage

    AppenderMemory$clear()

------------------------------------------------------------------------

### Method `set_buffer_size()`

Set the maximum size of the buffer

#### Usage

    AppenderMemory$set_buffer_size(x)

#### Arguments

- `x`:

  an `integer` scalar `>= 0`. Number of
  [LogEvents](https://s-fleck.github.io/lgr/reference/LogEvent.md) to
  buffer.

------------------------------------------------------------------------

### Method `set_should_flush()`

Set function that can trigger flushing the buffer

#### Usage

    AppenderMemory$set_should_flush(x)

#### Arguments

- `x`:

  A `function` with the single argument `event`. Setting `x` to `NULL`
  is a shortcut for `function(event) FALSE`. See active bindings.

------------------------------------------------------------------------

### Method `set_flush_on_exit()`

Should the buffer be flushed when the Appender is destroyed?

#### Usage

    AppenderMemory$set_flush_on_exit(x)

#### Arguments

- `x`:

  A `logical` scalar. See active bindings.

------------------------------------------------------------------------

### Method `set_flush_on_rotate()`

Should the buffer be flushed if `buffer_size` is exceeded?

#### Usage

    AppenderMemory$set_flush_on_rotate(x)

#### Arguments

- `x`:

  A `logical` scalar. See active bindings.

------------------------------------------------------------------------

### Method `set_flush_threshold()`

Set threshold that triggers flushing

#### Usage

    AppenderMemory$set_flush_threshold(level)

#### Arguments

- `level`:

  A `numeric` or `character`
  [threshold](https://s-fleck.github.io/lgr/reference/get_log_levels.md).
  See active bindings.

------------------------------------------------------------------------

### Method `show()`

Display the contents of the log table. Relies on the `$format_event`
method of the
[Layout](https://s-fleck.github.io/lgr/reference/Layout.md) attached to
this Appender.

#### Usage

    AppenderMemory$show(threshold = NA_integer_, n = 20L)

#### Arguments

- `threshold`:

  `character` or `integer` scalar. The minimum log level that should be
  displayed.

- `n`:

  `integer` scalar. Show only the last `n` log entries that match
  `threshold`.

------------------------------------------------------------------------

### Method [`format()`](https://rdrr.io/r/base/format.html)

#### Usage

    AppenderMemory$format(color = FALSE, ...)
