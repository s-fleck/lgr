# Format Log Events as Text

Format a [LogEvent](https://s-fleck.github.io/lgr/reference/LogEvent.md)
as human readable text using
[`format.LogEvent()`](https://s-fleck.github.io/lgr/reference/print.LogEvent.md),
which provides a quick and easy way to customize log messages. If you
need more control and flexibility, consider using
[LayoutGlue](https://s-fleck.github.io/lgr/reference/LayoutGlue.md)
instead.

## Format Tokens

This is the same list of format tokens as for
[`format.LogEvent()`](https://s-fleck.github.io/lgr/reference/print.LogEvent.md)

- `%t` The timestamp of the message, formatted according to
  `timestamp_fmt`

- `%l` the log level, lowercase `character` representation

- `%L` the log level, uppercase `character` representation

- `%k` the log level, first letter of lowercase `character`
  representation

- `%K` the log level, first letter of uppercase `character`
  representation

- `%n` the log level, `integer` representation

- `%g` the name of the logger

- `%p` the PID (process ID). Useful when logging code that uses multiple
  threads.

- `%c` the calling function

- `%m` the log message

- `%r` the raw log message (without string interpolation)

- `%f` all custom fields of `x` in a pseudo-JSON like format that is
  optimized for human readability and console output

- `%j` all custom fields of `x` in proper JSON. This requires that you
  have **jsonlite** installed and does not support colors as opposed to
  `%f`

## See also

Other Layouts:
[`Layout`](https://s-fleck.github.io/lgr/reference/Layout.md),
[`LayoutGlue`](https://s-fleck.github.io/lgr/reference/LayoutGlue.md),
[`LayoutJson`](https://s-fleck.github.io/lgr/reference/LayoutJson.md)

## Super class

[`Layout`](https://s-fleck.github.io/lgr/reference/Layout.md) -\>
`LayoutFormat`

## Active bindings

- `fmt`:

  a `character` scalar containing format tokens. See
  [`format.LogEvent()`](https://s-fleck.github.io/lgr/reference/print.LogEvent.md).

- `timestamp_fmt`:

  a `character` scalar. See
  [`base::format.POSIXct()`](https://rdrr.io/r/base/strptime.html).

- `colors`:

  a named `list` of functions (like the ones provided by the package
  crayon) passed on on
  [`format.LogEvent()`](https://s-fleck.github.io/lgr/reference/print.LogEvent.md).

- `pad_levels`:

  `"right"`, `"left"` or `NULL`. See
  [`format.LogEvent()`](https://s-fleck.github.io/lgr/reference/print.LogEvent.md).

## Methods

### Public methods

- [`LayoutFormat$new()`](#method-LayoutFormat-initialize)

- [`LayoutFormat$format_event()`](#method-LayoutFormat-format_event)

- [`LayoutFormat$set_fmt()`](#method-LayoutFormat-set_fmt)

- [`LayoutFormat$set_timestamp_fmt()`](#method-LayoutFormat-set_timestamp_fmt)

- [`LayoutFormat$set_colors()`](#method-LayoutFormat-set_colors)

- [`LayoutFormat$set_pad_levels()`](#method-LayoutFormat-set_pad_levels)

- [`LayoutFormat$toString()`](#method-LayoutFormat-toString)

- [`LayoutFormat$read()`](#method-LayoutFormat-read)

- [`LayoutFormat$clone()`](#method-LayoutFormat-clone)

Inherited methods

- [`Layout$set_excluded_fields()`](https://s-fleck.github.io/lgr/reference/Layout.html#method-set_excluded_fields)

------------------------------------------------------------------------

### `LayoutFormat$new()`

Create a new `LayoutFormat` object. All arguments are optional and
correspond to the identically named fields (see Active bindings).

#### Usage

    LayoutFormat$new(
      fmt = "%L [%t] %m %j",
      timestamp_fmt = "%Y-%m-%d %H:%M:%OS3",
      colors = NULL,
      pad_levels = "right",
      excluded_fields = NULL
    )

#### Arguments

- `fmt`:

  a `character` scalar containing format tokens. See
  [`format.LogEvent()`](https://s-fleck.github.io/lgr/reference/print.LogEvent.md).

- `timestamp_fmt`:

  a `character` scalar. See
  [`base::format.POSIXct()`](https://rdrr.io/r/base/strptime.html).

- `colors`:

  a named `list` of functions (like the ones provided by the package
  crayon) passed on on
  [`format.LogEvent()`](https://s-fleck.github.io/lgr/reference/print.LogEvent.md).

- `pad_levels`:

  `"right"`, `"left"` or `NULL`. See
  [`format.LogEvent()`](https://s-fleck.github.io/lgr/reference/print.LogEvent.md).

- `excluded_fields`:

  see the `excluded_fields` field of the parent
  [Layout](https://s-fleck.github.io/lgr/reference/Layout.md) class.

------------------------------------------------------------------------

### `LayoutFormat$format_event()`

Format a LogEvent

#### Usage

    LayoutFormat$format_event(event)

#### Arguments

- `event`:

  a [LogEvent](https://s-fleck.github.io/lgr/reference/LogEvent.md)

#### Returns

a `character` scalar

------------------------------------------------------------------------

### `LayoutFormat$set_fmt()`

Set the `fmt` field

#### Usage

    LayoutFormat$set_fmt(x)

#### Arguments

- `x`:

  see the `fmt` field

#### Returns

The LayoutFormat object, invisibly (for chaining).

------------------------------------------------------------------------

### `LayoutFormat$set_timestamp_fmt()`

Set the `timestamp_fmt` field

#### Usage

    LayoutFormat$set_timestamp_fmt(x)

#### Arguments

- `x`:

  see the `timestamp_fmt` field

#### Returns

The LayoutFormat object, invisibly (for chaining).

------------------------------------------------------------------------

### `LayoutFormat$set_colors()`

Set the `colors` field

#### Usage

    LayoutFormat$set_colors(x)

#### Arguments

- `x`:

  see the `colors` field

#### Returns

The LayoutFormat object, invisibly (for chaining).

------------------------------------------------------------------------

### `LayoutFormat$set_pad_levels()`

Set the `pad_levels` field

#### Usage

    LayoutFormat$set_pad_levels(x)

#### Arguments

- `x`:

  see the `pad_levels` field

#### Returns

The LayoutFormat object, invisibly (for chaining).

------------------------------------------------------------------------

### `LayoutFormat$toString()`

Convert Layout to a `character` string

#### Usage

    LayoutFormat$toString()

#### Returns

a `character` scalar

------------------------------------------------------------------------

### `LayoutFormat$read()`

Read a log file written using LayoutFormat

#### Usage

    LayoutFormat$read(file, threshold = NA_integer_, n = 20L)

#### Arguments

- `file`:

  `character` scalar. Path to the log file.

- `threshold`:

  a `character` or `integer` threshold

- `n`:

  number of log entries to display

#### Returns

a `character` vector of the (tail of the) matching log lines

------------------------------------------------------------------------

### `LayoutFormat$clone()`

The objects of this class are cloneable with this method.

#### Usage

    LayoutFormat$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# setup a dummy LogEvent
event <- LogEvent$new(
  logger = Logger$new("dummy logger"),
  level = 200,
  timestamp = Sys.time(),
  caller = NA_character_,
  msg = "a test message"
)
lo <- LayoutFormat$new()
lo$format_event(event)
#> [1] "ERROR [2026-07-21 14:17:47.692] a test message"
```
