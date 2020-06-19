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
  warning(
    "[", format(Sys.time(), format = "%Y-%m-%d %H:%M:%OS3"), "] ",
    "An error occurred during logging: ", e, call. = FALSE
  )
}
