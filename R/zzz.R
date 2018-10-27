# Loggers -----------------------------------------------------------------
#' @export
collector_default <- collector_dt$new(
  level = NA_integer_,
  timestamp = Sys.time,
  msg = NA_character_,
  caller = get_caller,
  .cache_size = 1e5
)



#' @export
collector_slim <- collector_dt$new(
  level = NA_integer_,
  timestamp = Sys.time,
  msg = NA_character_,
  .cache_size = 1e3
)




# appenders ---------------------------------------------------------------



#' @export
console_appender_slim <- appender_console_minimal$new()




#' @export
console_appender_color <- appender_console$new(
  colors = list(
    "fatal" = function(x) colt::clt_error(colt::clt_emph2(x)),
    "error" = colt::clt_error,
    "warn"  = colt::clt_warning,
    "debug" = colt::clt_chr,
    "trace" = colt::clt_chr_subtle
  )
)
