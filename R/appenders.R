appender_console <- function(x, formatter = formatter_simple){
  cat(formatter(x))
}
