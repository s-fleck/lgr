# Logger Error Conditions

Logger Error Conditions

## Usage

``` r
CannotInitializeAbstractClassError(class = parent.frame(2)[["classes"]])
```

## Arguments

- class:

  `character` scalar. The abstract class that was mistakenly tried to
  initialize. The default is to discover the class name automatically if
  called inside `$initialize(){...}` in an
  [R6::R6](https://r6.r-lib.org/reference/R6Class.html) class definition

## Value

a condition object
