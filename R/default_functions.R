# utils -------------------------------------------------------------------

#' Demote an Exception to a Warning
#'
#' Throws a timestamped warning instead of stopping the program. This is
#' the default exception handler used by [Loggers].
#'
#' @param e a `character` scalar, usually a `try-error` as thrown by
#' [base::tryCatch()]
#'
#' @return The warning as `character` vector
#' @export
#'
#' @examples
#' tryCatch(stop("an error has occurred"), error = default_exception_handler)
#'
default_exception_handler <- function(e){
  warning(
    "[", format(Sys.time(), format = "%Y-%m-%d %H:%M:%OS3"), "] ",
    "An error occurred during logging: ", e, call. = FALSE
  )
}




#' Default Should Flush Function
#'
#' This is the default "should flush" trigger function for Appenders that
#' support such a mechanism, such as [AppenderBuffer] and [AppenderDbi]. It
#' returns `TRUE` if the event's `level` meets or exceeds the Appender's
#' `flush_threshold`.
#'
#' @param event a [LogEvent]
#'
#' @return `TRUE` or `FALSE`
#' @export
#'
default_should_flush <- function(event){
  is.na(.obj()[["flush_threshold"]]) || all(event[["level"]] <= .obj()[["flush_threshold"]])
}
