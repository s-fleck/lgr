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
  ml
){
  paste0(
    toupper(ml$label_levels(x$level)),
    " [",
    x[["timestamp"]], "] ",
    x[["msg"]],
    collapse = "\n"
  )
}
