appender_console <- function(
  x,
  threshold,
  formatter,
  log_levels
){
  cat(formatter(x[x$level <= threshold], log_levels))
}
