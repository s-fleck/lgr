# Abstract Class for Layouts

Abstract Class for Layouts

Abstract Class for Layouts

## Details

[Appenders](https://s-fleck.github.io/lgr/reference/Appender.md) pass
[LogEvents](https://s-fleck.github.io/lgr/reference/LogEvent.md) to a
Layout which formats it for output. For the Layouts included in lgr that
means turning the LogEvent into a `character` string.

For each Appender exist one more more possible Layouts, but not every
Layout will work with every Appender. See the package lgrExtra for
examples for Layouts that return different data types (such as
`data.frames`) and Appenders that can handle them.

## Notes for developers

Layouts may have an additional `$read(file, threshold, n)` method that
returns a `character` vector, and/or an `$parse(file)` method that
returns a `data.frame`. These can be used by Appenders to `$show()`
methods and `$data` active bindings respectively (see source code of
[AppenderFile](https://s-fleck.github.io/lgr/reference/AppenderFile.md)).

## See also

Other Layouts:
[`LayoutFormat`](https://s-fleck.github.io/lgr/reference/LayoutFormat.md),
[`LayoutGlue`](https://s-fleck.github.io/lgr/reference/LayoutGlue.md),
[`LayoutJson`](https://s-fleck.github.io/lgr/reference/LayoutJson.md)

## Active bindings

- `excluded_fields`:

  fields to exclude from the final log

## Methods

### Public methods

- [`Layout$format_event()`](#method-Layout-format_event)

- [`Layout$toString()`](#method-Layout-toString)

- [`Layout$set_excluded_fields()`](#method-Layout-set_excluded_fields)

- [`Layout$clone()`](#method-Layout-clone)

------------------------------------------------------------------------

### Method `format_event()`

Format a log event

Function that the Layout uses to transform a
[LogEvent](https://s-fleck.github.io/lgr/reference/LogEvent.md) into
something that an
[Appender](https://s-fleck.github.io/lgr/reference/Appender.md) can
write to an output destination.

#### Usage

    Layout$format_event(event)

#### Arguments

- `event`:

  a [LogEvent](https://s-fleck.github.io/lgr/reference/LogEvent.md)

------------------------------------------------------------------------

### Method [`toString()`](https://rdrr.io/r/base/toString.html)

#### Usage

    Layout$toString()

------------------------------------------------------------------------

### Method `set_excluded_fields()`

#### Usage

    Layout$set_excluded_fields(x)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Layout$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
