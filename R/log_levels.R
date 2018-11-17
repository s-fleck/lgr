as_log_levels <- function(x){
  assert(is_integerish(x) && identical(length(names(x)), length(x)))
  assert(
    x > 0 && !is.na(x),
    "The log levels `0` (off) and `NA` (all) are reserved"
  )
  x <- setNames(as.integer(x), names(x))
  structure(sort(x), class = c("log_levels", "integer"))
}




#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
add_log_levels <- function(
  x
){
  current_lvls <- getOption("yog.log_levels")
  assert(
    !is.null(current_lvls),
    "yog.log_levels option is not set. something is very wrong with yog, please file a bug report"
  )
  x <- setNames(as.integer(x), names(x))
  res <- as_log_levels(c(current_lvls, x))
  options(yog.log_levels = res)
}





#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
remove_log_levels <- function(
  x
){
  assert(is.character(x))
  assert(
    !any(c("fatal", "error", "warn", "info", "debug", "trace") %in% x),
    "Cannot remove default log levels"
  )
  current_lvls <- getOption("yog.log_levels")
  assert(all(x) %in% names(current_lvls))
  res <- current_lvls[!names(current_lvls) %in% x]
  res <- as_log_levels(res)
  options(yog.log_levels = res)
}
