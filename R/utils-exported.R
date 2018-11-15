#' Title
#'
#' @param level
#' @param logger a [Logger] or [Appender]. Defaults to the root logger
#'
#' @return
#' @export
#'
#' @examples
threshold <- function(
  level,
  target = yog::yog
){
  if (missing(level))
    target$threshold
  else
    target$threshold <- level
}



#' Title
#'
#' @param appender
#' @param target
#'
#' @return
#' @export
#'
#' @examples
add_appender <- function(
  appender,
  target = yog::yog
){
  target$add_appender(appender)
}



#' Title
#'
#' @param pos
#' @param target
#'
#' @return
#' @export
#'
#' @examples
add_remove_appender <- function(
  pos,
  target = yog::yog
){
  target$remove_appender(appender)
}
