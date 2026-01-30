# Print Logger Trees

Print Logger Trees

## Usage

``` r
# S3 method for class 'logger_tree'
print(x, color = requireNamespace("crayon", quietly = TRUE), ...)

# S3 method for class 'logger_tree'
format(x, color = FALSE, ...)
```

## Arguments

- x:

  a
  [logger_tree](https://s-fleck.github.io/lgr/reference/logger_tree.md)

- color:

  `logical` scalar. If `TRUE` terminal output is colorized via the
  package crayon?

- ...:

  passed on to
  [`cli::tree()`](https://cli.r-lib.org/reference/tree.html)

## Value

`x` (invisibly)
