# Pad Character Vectors

Pad Character Vectors

## Arguments

- x:

  a `character` vector

- width:

  `integer` scalar. target string width

- pad:

  `character` scalar. the symbol to pad with

## Examples

``` r
pad_left("foo", 5)
#> [1] "  foo"
pad_right("foo", 5, ".")
#> [1] "foo.."
pad_left(c("foo", "foooooo"), pad = ".")
#> [1] "....foo" "foooooo"
```
