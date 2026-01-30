# Format Log Events as Text

Format Log Events as Text

Format Log Events as Text

## Details

Format a [LogEvent](https://s-fleck.github.io/lgr/reference/LogEvent.md)
as human readable text using
[`format.LogEvent()`](https://s-fleck.github.io/lgr/reference/print.LogEvent.md),
which provides a quick and easy way to customize log messages. If you
need more control and flexibility, consider using
[LayoutGlue](https://s-fleck.github.io/lgr/reference/LayoutGlue.md)
instead.

see Fields

see Fields

see Fields

see Fields

Convert Layout to a `character` string Read a log file written using
LayoutFormat

## Format Tokens

This is the same list of format tokens as for
[`format.LogEvent()`](https://s-fleck.github.io/lgr/reference/print.LogEvent.md)

- `%t`:

  The timestamp of the message, formatted according to `timestamp_fmt`)

- `%l`:

  the log level, lowercase `character` representation

- `%L`:

  the log level, uppercase `character` representation

- `%k`:

  the log level, first letter of lowercase `character` representation

- `%K`:

  the log level, first letter of uppercase `character` representation

- `%n`:

  the log level, `integer` representation

- `%g`:

  the name of the logger

- `%p`:

  the PID (process ID). Useful when logging code that uses multiple
  threads.

- `%c`:

  the calling function

- `%m`:

  the log message

- `%r`:

  the raw log message (without string interpolation)

- `%f`:

  all custom fields of `x` in a pseudo-JSON like format that is
  optimized for human readability and console output

- `%j`:

  all custom fields of `x` in proper JSON. This requires that you have
  **jsonlite** installed and does not support colors as opposed to `%f`

## See also

Other Layouts:
[`Layout`](https://s-fleck.github.io/lgr/reference/Layout.md),
[`LayoutGlue`](https://s-fleck.github.io/lgr/reference/LayoutGlue.md),
[`LayoutJson`](https://s-fleck.github.io/lgr/reference/LayoutJson.md)

## Super class

[`lgr::Layout`](https://s-fleck.github.io/lgr/reference/Layout.md) -\>
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

- [`LayoutFormat$new()`](#method-LayoutFormat-new)

- [`LayoutFormat$format_event()`](#method-LayoutFormat-format_event)

- [`LayoutFormat$set_fmt()`](#method-LayoutFormat-set_fmt)

- [`LayoutFormat$set_timestamp_fmt()`](#method-LayoutFormat-set_timestamp_fmt)

- [`LayoutFormat$set_colors()`](#method-LayoutFormat-set_colors)

- [`LayoutFormat$set_pad_levels()`](#method-LayoutFormat-set_pad_levels)

- [`LayoutFormat$toString()`](#method-LayoutFormat-toString)

- [`LayoutFormat$read()`](#method-LayoutFormat-read)

- [`LayoutFormat$clone()`](#method-LayoutFormat-clone)

Inherited methods

- [`lgr::Layout$set_excluded_fields()`](https://s-fleck.github.io/lgr/reference/Layout.html#method-set_excluded_fields)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    LayoutFormat$new(
      fmt = "%L [%t] %m %j",
      timestamp_fmt = "%Y-%m-%d %H:%M:%OS3",
      colors = NULL,
      pad_levels = "right",
      excluded_fields = NULL
    )

------------------------------------------------------------------------

### Method `format_event()`

Format a LogEvent

#### Usage

    LayoutFormat$format_event(event)

#### Arguments

- `event`:

  a [LogEvent](https://s-fleck.github.io/lgr/reference/LogEvent.md)

------------------------------------------------------------------------

### Method `set_fmt()`

#### Usage

    LayoutFormat$set_fmt(x)

------------------------------------------------------------------------

### Method `set_timestamp_fmt()`

#### Usage

    LayoutFormat$set_timestamp_fmt(x)

------------------------------------------------------------------------

### Method `set_colors()`

#### Usage

    LayoutFormat$set_colors(x)

------------------------------------------------------------------------

### Method `set_pad_levels()`

#### Usage

    LayoutFormat$set_pad_levels(x)

------------------------------------------------------------------------

### Method [`toString()`](https://rdrr.io/r/base/toString.html)

#### Usage

    LayoutFormat$toString()

------------------------------------------------------------------------

### Method `read()`

#### Usage

    LayoutFormat$read(file, threshold = NA_integer_, n = 20L)

#### Arguments

- `threshold`:

  a `character` or `integer` threshold

- `n`:

  number of log entries to display

------------------------------------------------------------------------

### Method `clone()`

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
#> [1] "ERROR [2026-01-30 13:43:53.878] a test message"
```
