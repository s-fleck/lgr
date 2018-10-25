#' Title
#'
#' a function that expects a named list with the elements
#' `level` `timestamp` `user` `pid` `caller` `msg` `level_name`
#'
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
formatter_simple <- function(
  x,
  log_levels
){
  paste0(
    toupper(names(log_levels)[match(x[["level"]], log_levels)]),
    " [",
    x[["timestamp"]], "] ",
    x[["msg"]],
    collapse = "\n"
  )
}
