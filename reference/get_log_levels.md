# Manage Log Levels

Display, add and remove character labels for log levels.

## Usage

``` r
get_log_levels()

add_log_levels(levels)

remove_log_levels(level_names)
```

## Arguments

- levels:

  a named `character` vector (see examples)

- level_names:

  a `character` vector of the names of the levels to remove

## Value

a named `character` vector of the globally available log levels
(`add_log_levels()` and `remove_log_levels()` return invisibly).

## Default Log Levels

lgr comes with the following predefined log levels that are identical to
the log levels of log4j.

|       |       |                                                                                                                              |
|-------|-------|------------------------------------------------------------------------------------------------------------------------------|
| Level | Name  | Description                                                                                                                  |
| `0`   | off   | A log level of 0/off tells a Logger or Appender to suspend all logging                                                       |
| `100` | fatal | Critical error that leads to program abort. Should always indicate a [`stop()`](https://rdrr.io/r/base/stop.html) or similar |
| `200` | error | A severe error that does not trigger program abort                                                                           |
| `300` | warn  | A potentially harmful situation, like [`warning()`](https://rdrr.io/r/base/warning.html)                                     |
| `400` | info  | An informational message on the progress of the application                                                                  |
| `500` | debug | Finer grained informational messages that are mostly useful for debugging                                                    |
| `600` | trace | An even finer grained message than debug                                                                                     |
| `NA`  | all   | A log level of NA/all tells a Logger or Appender to process all log events                                                   |

## Examples

``` r
get_log_levels()
#> fatal error  warn  info debug trace 
#>   100   200   300   400   500   600 
add_log_levels(c(errorish = 250))
get_log_levels()
#>    fatal    error errorish     warn     info    debug    trace 
#>      100      200      250      300      400      500      600 
#> attr(,"class")
#> [1] "log_levels" "integer"   
remove_log_levels("errorish")
get_log_levels()
#> fatal error  warn  info debug trace 
#>   100   200   300   400   500   600 
#> attr(,"class")
#> [1] "log_levels" "integer"   
```
