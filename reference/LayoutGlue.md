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

[`lgr::Layout`](https://s-fleck.github.io/lgr/reference/Layout.md) -\>
`LayoutGlue`

## Active bindings

- `fmt`:

  A string that will be interpreted by
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html)

## Methods

### Public methods

- [`LayoutGlue$new()`](#method-LayoutGlue-new)

- [`LayoutGlue$format_event()`](#method-LayoutGlue-format_event)

- [`LayoutGlue$set_fmt()`](#method-LayoutGlue-set_fmt)

- [`LayoutGlue$set_colors()`](#method-LayoutGlue-set_colors)

- [`LayoutGlue$toString()`](#method-LayoutGlue-toString)

- [`LayoutGlue$clone()`](#method-LayoutGlue-clone)

Inherited methods

- [`lgr::Layout$set_excluded_fields()`](https://s-fleck.github.io/lgr/reference/Layout.html#method-set_excluded_fields)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    LayoutGlue$new(
      fmt = "{pad_right(colorize_levels(toupper(level_name)), 5)} [{timestamp}] {msg}"
    )

------------------------------------------------------------------------

### Method `format_event()`

#### Usage

    LayoutGlue$format_event(event)

------------------------------------------------------------------------

### Method `set_fmt()`

#### Usage

    LayoutGlue$set_fmt(x)

------------------------------------------------------------------------

### Method `set_colors()`

#### Usage

    LayoutGlue$set_colors(x)

------------------------------------------------------------------------

### Method [`toString()`](https://rdrr.io/r/base/toString.html)

#### Usage

    LayoutGlue$toString()

------------------------------------------------------------------------

### Method `clone()`

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
#> FATAL [2026-01-30 11:27:35.640985] test


# All fields of the LogEvent are available, even custom ones
lg$appenders[[1]]$layout$set_fmt(
  "{logger} {level_name}({level}) {caller}: {toupper(msg)} {{custom: {custom}}}"
)
lg$fatal("test", custom = "foobar")
#> test fatal(100) eval: TEST {custom: foobar}
lg$config(NULL)  # reset logger config
#> <Logger> [info] test
#> 
#> inherited appenders:
#>   console: <AppenderConsole> [all] -> console
```
