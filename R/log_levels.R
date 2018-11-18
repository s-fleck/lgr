#' Manage Log Levels
#'
#' Display, add and remove character labels for log levels.
#'
#' @return a named `character` vector of the globally available log levels
#'   (`add_log_levels()` and `remove_log_levels()` return invisibly).
#' @export
#'
#' @examples
#' get_log_levels()
#' add_log_levels(c(errorish = 250))
#' get_log_levels()
#' remove_log_levels("errorish")
#' get_log_levels()
#'
get_log_levels <- function(){
  getOption("yog.log_levels")
}




#' @param x a named `character` vector (see examples)
#' @rdname get_log_levels
#' @export
add_log_levels <- function(
  levels
){
  current_lvls <- getOption("yog.log_levels")
  assert(
    !is.null(current_lvls),
    "yog.log_levels option is not set. something is very wrong with yog, please file a bug report"
  )
  levels <- setNames(as.integer(levels), names(levels))
  res <- as_log_levels(c(current_lvls, levels))
  options(yog.log_levels = res)
  invisible(get_log_levels())
}




#' @param name a `character` vector of the names of the levels to remove
#' @rdname get_log_levels
#' @export
remove_log_levels <- function(
  name
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
  invisible(get_log_levels())
}




as_log_levels <- function(x){
  assert(is_integerish(x) && identical(length(names(x)), length(x)))
  assert(
    x > 0 && !is.na(x),
    "The log levels `0` (off) and `NA` (all) are reserved"
  )
  x <- setNames(as.integer(x), names(x))
  structure(sort(x), class = c("log_levels", "integer"))
}




format_log_levels <- function(
  x
){
  paste0(names(sort(x)), " (", sort(x), ")", collapse = ", ")
}
