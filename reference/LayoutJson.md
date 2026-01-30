# Format LogEvents as JSON

A format for formatting LogEvents as [jsonlines](https://jsonlines.org/)
log files. This provides a nice balance between human- an
machine-readability.

## See also

[`read_json_lines()`](https://s-fleck.github.io/lgr/reference/read_json_lines.md),
<https://jsonlines.org/>

Other Layouts:
[`Layout`](https://s-fleck.github.io/lgr/reference/Layout.md),
[`LayoutFormat`](https://s-fleck.github.io/lgr/reference/LayoutFormat.md),
[`LayoutGlue`](https://s-fleck.github.io/lgr/reference/LayoutGlue.md)

## Super class

[`lgr::Layout`](https://s-fleck.github.io/lgr/reference/Layout.md) -\>
`LayoutJson`

## Active bindings

- `toJSON_args`:

  a `list`

- `timestamp_fmt`:

  a `character` scalar or a `function` that accepts a `POSIXct` as its
  single argument

- `transform_event`:

  a `function` that accepts a `LogEvent` as its single argument

- `transform_event_names`:

  a named `character` vector or a function that accepts a `character`
  vector of field names as its single argument.

## Methods

### Public methods

- [`LayoutJson$new()`](#method-LayoutJson-new)

- [`LayoutJson$format_event()`](#method-LayoutJson-format_event)

- [`LayoutJson$set_toJSON_args()`](#method-LayoutJson-set_toJSON_args)

- [`LayoutJson$set_timestamp_fmt()`](#method-LayoutJson-set_timestamp_fmt)

- [`LayoutJson$set_transform_event()`](#method-LayoutJson-set_transform_event)

- [`LayoutJson$set_transform_event_names()`](#method-LayoutJson-set_transform_event_names)

- [`LayoutJson$toString()`](#method-LayoutJson-toString)

- [`LayoutJson$parse()`](#method-LayoutJson-parse)

- [`LayoutJson$read()`](#method-LayoutJson-read)

- [`LayoutJson$clone()`](#method-LayoutJson-clone)

Inherited methods

- [`lgr::Layout$set_excluded_fields()`](https://s-fleck.github.io/lgr/reference/Layout.html#method-set_excluded_fields)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    LayoutJson$new(
      toJSON_args = list(auto_unbox = TRUE),
      timestamp_fmt = NULL,
      transform_event = function(event) event[["values"]],
      transform_event_names = NULL,
      excluded_fields = "rawMsg"
    )

#### Arguments

- `toJSON_args`:

  a list of arguments passed to
  [`jsonlite::toJSON()`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html),

- `timestamp_fmt`:

  Format to be applied to the timestamp. This is applied after
  `transform_event` but `before transform_event_names`

  - `NULL`: formatting of the timestamp is left to
    [`jsonlite::toJSON()`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html),

  - a `character` scalar as for
    [`format.POSIXct()`](https://rdrr.io/r/base/strptime.html), or

  - a `function` that returns a vector of the same length as its
    ([POSIXct](https://rdrr.io/r/base/DateTimeClasses.html)) input. The
    returned vector can be of any type supported by
    [`jsonlite::toJSON()`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html).

- `transform_event`:

  a `function` with a single argument that takes a
  [LogEvent](https://s-fleck.github.io/lgr/reference/LogEvent.md) object
  and returns a `list` of values.

- `transform_event_names`:

  - `NULL`: don't process names

  - a named `character` vector of the format `new_name = old_name`

  - or a `function` with a single mandatory argument that accepts a
    `character` vector of field names. Applied after `transform_event`.

- `excluded_fields`:

  A `character` vector of field names to exclude from the final output.
  Applied after `transform_event_names`.

------------------------------------------------------------------------

### Method `format_event()`

#### Usage

    LayoutJson$format_event(event)

------------------------------------------------------------------------

### Method `set_toJSON_args()`

#### Usage

    LayoutJson$set_toJSON_args(x)

#### Arguments

- `x`:

  a `list`

------------------------------------------------------------------------

### Method `set_timestamp_fmt()`

#### Usage

    LayoutJson$set_timestamp_fmt(x)

#### Arguments

- `x`:

  a `character` scalar or a `function` that accepts a `POSIXct` as its
  single argument

------------------------------------------------------------------------

### Method `set_transform_event()`

#### Usage

    LayoutJson$set_transform_event(x)

#### Arguments

- `x`:

  a `function` that accepts a `LogEvent` as its single argument

------------------------------------------------------------------------

### Method `set_transform_event_names()`

#### Usage

    LayoutJson$set_transform_event_names(x)

#### Arguments

- `x`:

  a named `character` vector or a function that accepts a `character`
  vector of field names as its single argument.

------------------------------------------------------------------------

### Method [`toString()`](https://rdrr.io/r/base/toString.html)

Represent the `LayoutJson` class as a string

#### Usage

    LayoutJson$toString()

------------------------------------------------------------------------

### Method [`parse()`](https://rdrr.io/r/base/parse.html)

Read and parse file written using this Layout

This can be used by the `$data` active binding of an
[Appender](https://s-fleck.github.io/lgr/reference/Appender.md)

#### Usage

    LayoutJson$parse(file)

#### Arguments

- `file`:

  `character` scalar: path to a file

------------------------------------------------------------------------

### Method `read()`

Read a file written using this Layout (without parsing)

This can be used by the `$show()` method of an
[Appender](https://s-fleck.github.io/lgr/reference/Appender.md)

#### Usage

    LayoutJson$read(file, threshold = NA_integer_, n = 20L)

#### Arguments

- `file`:

  `character` scalar: path to a file

- `threshold`:

  `character` Minimum log level to show. Requires parsing of the log
  file (but will still display unparsed output)

- `n`:

  `integer` number of lines to show

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LayoutJson$clone(deep = FALSE)

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
  msg = "a test message",
  custom_field = "LayoutJson can handle arbitrary fields"
)

lo <- LayoutJson$new()
lo$format_event(event)
#> {"level":200,"timestamp":"2026-01-30 11:47:31","logger":"dummy logger","caller":null,"msg":"a test message","custom_field":"LayoutJson can handle arbitrary fields"} 

lo <- LayoutJson$new(
  transform_event_names = toupper,
  excluded_fields = c("RAWMSG", "CALLER"))

lo$format_event(event)
#> {"LEVEL":200,"TIMESTAMP":"2026-01-30 11:47:31","LOGGER":"dummy logger","MSG":"a test message","CUSTOM_FIELD":"LayoutJson can handle arbitrary fields"} 

lo <- LayoutJson$new(
  transform_event = function(e) {
    values <- e$values
    values$msg <- toupper(values$msg)
    values
  },
  timestamp_fmt = "%a %b %d %H:%M:%S %Y",
  excluded_fields = c("RAWMSG", "CALLER"))

lo$format_event(event)
#> {"level":200,"timestamp":"Fri Jan 30 11:47:31 2026","logger":"dummy logger","caller":null,"msg":"A TEST MESSAGE","rawMsg":"a test message","custom_field":"LayoutJson can handle arbitrary fields"} 
```
