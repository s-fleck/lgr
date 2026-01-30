# Colorize Levels

Colorize Levels

## Usage

``` r
colorize_levels(
  x,
  colors = getOption("lgr.colors", NULL),
  transform = identity
)
```

## Arguments

- x:

  `numeric` or `character` levels to be colored. Unlike in many other
  functions in lgr, `character` levels are *not* case sensitive in this
  function and leading/trailing whitespace is ignored to make it more
  comfortable to use `colorize_levels()` inside formatting functions.

- colors:

  A `list` of `functions` that will be used to color the log levels
  (likely from
  [crayon::crayon](http://r-lib.github.io/crayon/reference/crayon.md)).

- transform:

  a `function` to transform `x` (for example
  [`toupper()`](https://rdrr.io/r/base/chartr.html))

## Value

a `character` vector wit color ANSI codes

## See also

Other formatting utils:
[`label_levels()`](https://s-fleck.github.io/lgr/reference/label_levels.md)

## Examples

``` r
cat(colorize_levels(c(100, 200)))
#> 100 200
cat(colorize_levels(c("trace", "warn ", "DEBUG")))
#> trace warn  DEBUG
cat(colorize_levels(c("trace", "warn ", "DEBUG"), transform = function(x) strtrim(x, 1) ))
#> trace warn  DEBUG
```
