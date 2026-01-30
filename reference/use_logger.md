# Setup a Simple Logger for a Package

This gives you a minimal logger with no appenders that you can use
inside your package under the name `lg` (e.g. lg\$fatal("test")).
`use_logger()` does not modify any files but only prints code for you to
copy and paste.

## Usage

``` r
use_logger(
  pkg = desc::desc_get("Package", rprojroot::find_package_root_file("DESCRIPTION"))[[1]]
)
```

## Arguments

- pkg:

  `character` scalar. Name of the package. The default is to try to get
  the Package name automatically using the packages **rprojroot** and
  **desc**

## Value

a `character` scalar containing R code.

## Examples

``` r
use_logger("testpkg")
#> 
#> Add the following to any R file in your package (usually 'testpkg-package.R' or 'zzz.R'):
#> .onLoad <- function(...){
#>   assign(
#>     "lg",
#>     lgr::get_logger("testpkg"),
#>     envir = parent.env(environment())
#>   )
#> }
```
