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
formatter_simple <- function(x){
  paste0(toupper(as.character(x[["level_name"]])), " [", x[["timestamp"]], "] ", x[["msg"]], collapse = "\n")
}
