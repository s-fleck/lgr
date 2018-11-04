#' Title
#'
#' @param fmt
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
sprintf_safely <- function(
  fmt,
  ...
){
  tryCatch(
    sprintf(fmt, ...),
    error = function(e) paste("LOGGING ERROR!!!:", trimws(e))
  )
}
