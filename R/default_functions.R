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
  ts <- paste0("[", format(Sys.time(), format = "%Y-%m-%d %H:%M:%OS3"), "]")

  if ("appender" %in% names(e)){
    res <- appendingFailedWarning(
      paste(ts, paste(class_fmt(e$appender, ignore = c("Filterable", "R6", "Appender")), "encountered an error:", e$message)),
      error = e
    )
  } else {
    res <- loggingFailedWarning(
      paste(ts, "an error occurred during logging:", e$message),
      error = e
    )
  }
  warning(res)
}



appendingFailedWarning <- function(message, class = NULL, call = NULL, ...){
  warning_condition(
    message = message,
    class = union(class, "appendingFailedWarning"),
    call = call,
    ...
  )
}



loggingFailedWarning <- function(message, class = NULL, call = NULL, ...){
  warning_condition(
    message = message,
    class = union(class, "loggingFailedWarning"),
    call = call,
    ...
  )
}
