# Appenders

Appenders are attached to
[Loggers](https://s-fleck.github.io/lgr/reference/Logger.md) and manage
the output of the
[LogEvents](https://s-fleck.github.io/lgr/reference/LogEvent.md) to a
destination - such as the console or a text file. An Appender has a
single [Layout](https://s-fleck.github.io/lgr/reference/Layout.md) that
tells it how to format the LogEvent. For details please refer to the
documentations of the specific Appenders.

Additional Appenders that support a wide range of output destinations -
such as databases, email, push-notifications or Linux syslog - are
available from the package
[lgrExtra](https://github.com/s-fleck/lgrExtra).

**NOTE**: This is an *abstract class*. Abstract classes cannot be
instantiated directly, but are exported for package developers that want
to extend lgr - for example by creating their own Appenders or
[Layouts](https://s-fleck.github.io/lgr/reference/Layout.md). Please
refer to the *see also* section for actual implementations of this
class.

## See also

Other abstract classes:
[`AppenderMemory`](https://s-fleck.github.io/lgr/reference/AppenderMemory.md),
[`AppenderTable`](https://s-fleck.github.io/lgr/reference/AppenderTable.md),
[`Filterable`](https://s-fleck.github.io/lgr/reference/Filterable.md)

Other Appenders:
[`AppenderBuffer`](https://s-fleck.github.io/lgr/reference/AppenderBuffer.md),
[`AppenderConsole`](https://s-fleck.github.io/lgr/reference/AppenderConsole.md),
[`AppenderFile`](https://s-fleck.github.io/lgr/reference/AppenderFile.md),
[`AppenderFileRotating`](https://s-fleck.github.io/lgr/reference/AppenderFileRotating.md),
[`AppenderFileRotatingDate`](https://s-fleck.github.io/lgr/reference/AppenderFileRotatingDate.md),
[`AppenderFileRotatingTime`](https://s-fleck.github.io/lgr/reference/AppenderFileRotatingTime.md),
[`AppenderTable`](https://s-fleck.github.io/lgr/reference/AppenderTable.md)

## Super class

[`Filterable`](https://s-fleck.github.io/lgr/reference/Filterable.md)
-\> `Appender`

## Active bindings

- `threshold`:

  `integer` scalar. The minimum log level that triggers this Appender.
  See `$set_threshold()`.

- `layout`:

  The [Layout](https://s-fleck.github.io/lgr/reference/Layout.md) that
  this Appender uses to format
  [LogEvents](https://s-fleck.github.io/lgr/reference/LogEvent.md). See
  `$set_layout()`.

- `destination`:

  The output destination of the `Appender` in human-readable form. This
  is mainly used when printing information about the Appender itself.

## Methods

### Public methods

- [`Appender$new()`](#method-Appender-initialize)

- [`Appender$append()`](#method-Appender-append)

- [`Appender$set_threshold()`](#method-Appender-set_threshold)

- [`Appender$set_layout()`](#method-Appender-set_layout)

- [`Appender$format()`](#method-Appender-format)

Inherited methods

- [`Filterable$add_filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-add_filter)
- [`Filterable$filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-filter)
- [`Filterable$remove_filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-remove_filter)
- [`Filterable$set_filters()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-set_filters)

------------------------------------------------------------------------

### `Appender$new()`

Create a new Appender. Appenders are usually not instantiated directly,
but through one of their non-abstract subclasses (such as
[AppenderConsole](https://s-fleck.github.io/lgr/reference/AppenderConsole.md)
or
[AppenderFile](https://s-fleck.github.io/lgr/reference/AppenderFile.md)).

#### Usage

    Appender$new(layout = Layout$new(), threshold = NA_integer_)

#### Arguments

- `layout`:

  A [Layout](https://s-fleck.github.io/lgr/reference/Layout.md) that
  determines how
  [LogEvents](https://s-fleck.github.io/lgr/reference/LogEvent.md) are
  formatted before being output by this Appender.

- `threshold`:

  `character` or `integer` scalar. The minimum log level that triggers
  this Appender. See
  [log_levels](https://s-fleck.github.io/lgr/reference/get_log_levels.md).

------------------------------------------------------------------------

### `Appender$append()`

Process a
[LogEvent](https://s-fleck.github.io/lgr/reference/LogEvent.md) `event`.
This method is usually not called by the user, but invoked by a
[Logger](https://s-fleck.github.io/lgr/reference/Logger.md)

#### Usage

    Appender$append(event)

#### Arguments

- `event`:

  a [LogEvent](https://s-fleck.github.io/lgr/reference/LogEvent.md)

------------------------------------------------------------------------

### `Appender$set_threshold()`

Set the minimum log level that triggers this Appender. See
[`threshold()`](https://s-fleck.github.io/lgr/reference/simple_logging.md)
for examples

#### Usage

    Appender$set_threshold(level)

#### Arguments

- `level`:

  `character` or `integer` scalar log level. See
  [log_levels](https://s-fleck.github.io/lgr/reference/get_log_levels.md).

------------------------------------------------------------------------

### `Appender$set_layout()`

Set the `Layout` that this Appender will use for formatting `LogEvents`

#### Usage

    Appender$set_layout(layout)

#### Arguments

- `layout`:

  A [Layout](https://s-fleck.github.io/lgr/reference/Layout.md) that
  determines how
  [LogEvents](https://s-fleck.github.io/lgr/reference/LogEvent.md) are
  formatted before being output by this Appender.

------------------------------------------------------------------------

### `Appender$format()`

Format the Appender for printing (see also
[`print.Appender()`](https://s-fleck.github.io/lgr/reference/print.Appender.md)).

#### Usage

    Appender$format(color = FALSE, ...)

#### Arguments

- `color`:

  `TRUE` or `FALSE`. Output with color? Requires the package **crayon**.

- `...`:

  ignored
