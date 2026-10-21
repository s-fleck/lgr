# Format Log Events as Text via glue

Format a [LogEvent](https://s-fleck.github.io/lgr/reference/LogEvent.md)
as human readable text using
[glue::glue](https://glue.tidyverse.org/reference/glue.html). The
function is evaluated in an environment in which it has access to all
elements of the
[LogEvent](https://s-fleck.github.io/lgr/reference/LogEvent.md) (see
examples). This is more flexible than
[LayoutFormat](https://s-fleck.github.io/lgr/reference/LayoutFormat.md),
but also more complex and slightly less performant.

## See also

lgr exports a number of formatting utility functions that are useful for
layout glue:
[`colorize_levels()`](https://s-fleck.github.io/lgr/reference/colorize_levels.md),
[`pad_left()`](https://s-fleck.github.io/lgr/reference/pad_right.md),
[`pad_right()`](https://s-fleck.github.io/lgr/reference/pad_right.md).

Other Layouts:
[`Layout`](https://s-fleck.github.io/lgr/reference/Layout.md),
[`LayoutFormat`](https://s-fleck.github.io/lgr/reference/LayoutFormat.md),
[`LayoutJson`](https://s-fleck.github.io/lgr/reference/LayoutJson.md)

## Super class

[`Layout`](https://s-fleck.github.io/lgr/reference/Layout.md) -\>
`LayoutGlue`

## Active bindings

- `fmt`:

  A string that will be interpreted by
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html)

## Methods

### Public methods

- [`LayoutGlue$new()`](#method-LayoutGlue-initialize)

- [`LayoutGlue$format_event()`](#method-LayoutGlue-format_event)

- [`LayoutGlue$set_fmt()`](#method-LayoutGlue-set_fmt)

- [`LayoutGlue$set_colors()`](#method-LayoutGlue-set_colors)

- [`LayoutGlue$toString()`](#method-LayoutGlue-toString)

- [`LayoutGlue$clone()`](#method-LayoutGlue-clone)

Inherited methods

- [`Layout$set_excluded_fields()`](https://s-fleck.github.io/lgr/reference/Layout.html#method-set_excluded_fields)

------------------------------------------------------------------------

### `LayoutGlue$new()`

Create a new `LayoutGlue` object. Requires the package glue.

#### Usage

    LayoutGlue$new(
      fmt = "{pad_right(colorize_levels(toupper(level_name)), 5)} [{timestamp}] {msg}"
    )

#### Arguments

- `fmt`:

  A string that will be interpreted by
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html)

------------------------------------------------------------------------

### `LayoutGlue$format_event()`

Format a LogEvent using
[`glue::glue()`](https://glue.tidyverse.org/reference/glue.html)

#### Usage

    LayoutGlue$format_event(event)

#### Arguments

- `event`:

  a [LogEvent](https://s-fleck.github.io/lgr/reference/LogEvent.md)

#### Returns

a `character` scalar

------------------------------------------------------------------------

### `LayoutGlue$set_fmt()`

Set the `fmt` field

#### Usage

    LayoutGlue$set_fmt(x)

#### Arguments

- `x`:

  see the `fmt` field

#### Returns

The LayoutGlue object, invisibly (for chaining).

------------------------------------------------------------------------

### `LayoutGlue$set_colors()`

Store a `list` of coloring functions for later use. `LayoutGlue` does
not apply these automatically within `$format_event()` — colorize log
levels directly in the glue template instead (e.g. via
[`colorize_levels()`](https://s-fleck.github.io/lgr/reference/colorize_levels.md),
see the class examples).

#### Usage

    LayoutGlue$set_colors(x)

#### Arguments

- `x`:

  `NULL`, or a `list` of `functions` (like the ones provided by the
  package crayon).

#### Returns

The LayoutGlue object, invisibly (for chaining).

------------------------------------------------------------------------

### `LayoutGlue$toString()`

Represent the Layout as a `character` scalar

#### Usage

    LayoutGlue$toString()

#### Returns

a `character` scalar

------------------------------------------------------------------------

### `LayoutGlue$clone()`

The objects of this class are cloneable with this method.

#### Usage

    LayoutGlue$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
lg <- get_logger("test")$
  set_appenders(AppenderConsole$new())$
  set_propagate(FALSE)

lg$appenders[[1]]$set_layout(LayoutGlue$new())
lg$fatal("test")
#> FATAL [2026-10-21 21:16:06.216085] test


# All fields of the LogEvent are available, even custom ones
lg$appenders[[1]]$layout$set_fmt(
  "{logger} {level_name}({level}) {caller}: {toupper(msg)} {{custom: {custom}}}"
)
lg$fatal("test", custom = "foobar")
#> test fatal(100) eval: TEST {custom: foobar}
lg$config(NULL)  # reset logger config
```
