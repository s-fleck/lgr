# utils -------------------------------------------------------------------

#' Demote an exception to a warning
#'
#' Throws a timestamped warning instead of stopping the program. This is
#' the default exception handler used by [Loggers].
#'
#' @param e an `error condition` object
#'
#' @return The warning as `character` vector
#' @export
#'
#' @examples
#' tryCatch(stop("an error has occurred"), error = default_exception_handler)
#'
default_exception_handler <- function(e){
  ts <- format(Sys.time(), format = "%Y-%m-%d %H:%M:%OS3")
  call <- paste(trimws(format(e$call)), collapse = " ")
  logger <- format(e$logger$name)

  if ("appender" %in% names(e)){
    appender <- paste0(class_fmt(e$appender, ignore = c("Filterable", "R6", "Appender")))
    message <- sprintf("[%s] %s %s ~ error in `%s`: %s", ts, logger, appender, call, e$message)
    res <- AppenderWarning(message = message, error = e)

  } else {
    message <- sprintf("[%s] %s ~ error in `%s`: %s", ts, logger, call, e$message)
    res <- LoggerWarning(message = message, error = e)
  }
  warning(res)
}



AppenderWarning <- function(message, class = NULL, call = NULL, ...){
  LoggerWarning(
    message = message,
    class = union(class, "AppenderWarning"),
    call = call,
    ...
  )
}



LoggerWarning <- function(message, class = NULL, call = NULL, ...){
  warning_condition(
    message = message,
    class = union(class, "LoggerWarning"),
    call = call,
    ...
  )
}
