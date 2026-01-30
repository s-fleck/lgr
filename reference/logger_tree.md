# Logger Tree

Displays a tree structure of all registered Loggers.

## Usage

``` r
logger_tree()
```

## Value

`data.frame` with subclass `"logger_tree"`

## Symbology

- unconfigured Loggers are displayed in gray (if your terminal supports
  colors and you have the package crayon installed).

- If a logger's `threshold` is set, it is displayed in square brackets
  next to its name (reminder: if the threshold is not set, it is
  inherited from next logger up the logger tree).

- If a logger's `propagate` field is set to `FALSE` an red hash (`#`)
  sign is displayed in front of the logger name, to imply that it does
  not pass LogEvents up the tree.

## See also

[`logger_index()`](https://s-fleck.github.io/lgr/reference/logger_index.md)
for a tidy `data.frame` representation of all registered loggers

## Examples

``` r
get_logger("fancymodel")
#> <Logger> [info] fancymodel
#> 
#> inherited appenders:
#>   console: <AppenderConsole> [all] -> console
get_logger("fancymodel/shiny")$
  set_propagate(FALSE)

get_logger("fancymodel/shiny/ui")$
  set_appenders(AppenderConsole$new())

get_logger("fancymodel/shiny/server")$
  set_appenders(list(AppenderConsole$new(), AppenderConsole$new()))$
  set_threshold("trace")

get_logger("fancymodel/plumber")
#> <Logger> [info] fancymodel/plumber
#> 
#> inherited appenders:
#>   console: <AppenderConsole> [all] -> console

if (requireNamespace("cli")){
  print(logger_tree())
}
#> root [info] -> 1 appender          
#> ├─fancymodel                       
#> │ ├─plumber                        
#> │ └─#shiny                         
#> │   ├─server [trace] -> 2 appenders
#> │   └─ui -> 1 appender             
#> ├─glue                             
#> ├─lg                               
#> │ └─child                          
#> ├─log                              
#> │ └─ger                            
#> │   └─test                         
#> ├─mylogger                         
#> ├─plant                            
#> │ └─shrub                          
#> │   └─leaf                         
#> ├─shrub                            
#> │ └─leaf                           
#> ├─test                             
#> └─tree                             
#>   └─leaf                           
```
