# Label/Unlabel Log Levels

Label/Unlabel Log Levels

## Usage

``` r
label_levels(levels, log_levels = getOption("lgr.log_levels"))

unlabel_levels(labels, log_levels = getOption("lgr.log_levels"))
```

## Arguments

- levels:

  an `integer` vector of log levels

- log_levels:

  named `integer` vector of valid log levels

- labels:

  a `character` vector of log level labels. Please note that log levels
  are lowercase by default, even if many appenders print them in
  uppercase.

## Value

a `character` vector for `label_levels()` and an integer vector for
`unlabel_levels`

## See also

[`get_log_levels()`](https://s-fleck.github.io/lgr/reference/get_log_levels.md)

Other formatting utils:
[`colorize_levels()`](https://s-fleck.github.io/lgr/reference/colorize_levels.md)

## Examples

``` r
x <- label_levels(c(seq(0, 600, by = 100), NA))
print(x)
#>       0     100     200     300     400     500     600    <NA> 
#>   "off" "fatal" "error"  "warn"  "info" "debug" "trace"   "all" 
unlabel_levels(x)
#>   off fatal error  warn  info debug trace   all 
#>     0   100   200   300   400   500   600    NA 
```
