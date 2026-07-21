# Return a data.frame of all registered loggers

Return a data.frame of all registered loggers

## Usage

``` r
logger_index()
```

## Value

a `logger_index` `data.frame`

## See also

[`logger_tree()`](https://s-fleck.github.io/lgr/reference/logger_tree.md)
for a more visual representation of registered loggers

## Examples

``` r
get_logger("tree/leaf")
#> <Logger> [info] tree/leaf
#> 
#> inherited appenders:
#>   console: <AppenderConsole> [all] -> console
get_logger("shrub/leaf")
#> <Logger> [info] shrub/leaf
#> 
#> inherited appenders:
#>   console: <AppenderConsole> [all] -> console
get_logger("plant/shrub/leaf")
#> <Logger> [info] plant/shrub/leaf
#> 
#> inherited appenders:
#>   console: <AppenderConsole> [all] -> console
logger_index()
#>                name configured threshold threshold_inherited propagate
#> 1              root       TRUE       400               FALSE      TRUE
#> 2              glue      FALSE       400                TRUE      TRUE
#> 3                lg      FALSE       400                TRUE      TRUE
#> 4          lg/child      FALSE       400                TRUE      TRUE
#> 5               log      FALSE       400                TRUE      TRUE
#> 6           log/ger      FALSE       400                TRUE      TRUE
#> 7      log/ger/test      FALSE       400                TRUE      TRUE
#> 8          mylogger      FALSE       400                TRUE      TRUE
#> 9             plant      FALSE       400                TRUE      TRUE
#> 10      plant/shrub      FALSE       400                TRUE      TRUE
#> 11 plant/shrub/leaf      FALSE       400                TRUE      TRUE
#> 12            shrub      FALSE       400                TRUE      TRUE
#> 13       shrub/leaf      FALSE       400                TRUE      TRUE
#> 14             test      FALSE       400                TRUE      TRUE
#> 15             tree      FALSE       400                TRUE      TRUE
#> 16        tree/leaf      FALSE       400                TRUE      TRUE
#>    n_appenders
#> 1            1
#> 2            0
#> 3            0
#> 4            0
#> 5            0
#> 6            0
#> 7            0
#> 8            0
#> 9            0
#> 10           0
#> 11           0
#> 12           0
#> 13           0
#> 14           0
#> 15           0
#> 16           0
```
