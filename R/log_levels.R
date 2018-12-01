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




#' @param names a `character` vector of the names of the levels to remove
#' @rdname get_log_levels
#' @export
remove_log_levels <- function(
  level_names
){
  assert(is.character(level_names))
  assert(
    !any(c("fatal", "error", "warn", "info", "debug", "trace") %in% level_names),
    "Cannot remove default log levels"
  )
  current_lvls <- getOption("yog.log_levels")
  assert(all(level_names %in% names(current_lvls)))
  res <- current_lvls[!names(current_lvls) %in% level_names]
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





#' Colorize Levels
#'
#' @param x levels to be colored
#' @param num_levels numeric version of x (to match against numeric color levels)
#' @param colors named list of coloring functions. the names should be the levels in x
#' @param num_levels_colors numeric version of the names of `colors` to be matched
#'   against num_levels
#'
#' @return a `character` vector wit color ansi codes
#' @noRd
#'
colorize_levels <- function(
  x,
  num_levels,
  colors,
  num_levels_colors
){
  if (is.null(colors))
    return(x)

  for (i in seq_along(colors)){
    sel <- num_levels == num_levels_colors[[i]]
    x[sel] <- colors[[i]](x[sel])
  }

  x
}




#' Label/Unlabel Log Levels
#'
#' @param levels an `integer` vector of log levels
#' @param labels a `character` vectpr of log level labels. Please note that
#'   log levels are lowercase by default, even if many appenders print them
#'   in uppercase.
#' @param log_levels a named `integer` vector, should usually not be set
#'   manually.
#'
#' @seealso also see [get_log_levels]
#' @return
#' a `character` vector for `label_levels()` and an integer vector for
#' `unlabel_levels`
#'
#' @export
#' @examples
#'
#' x <- label_levels(c(seq(0, 600, by = 100), NA))
#' print(x)
#' unlabel_levels(x)
#'
label_levels = function(
  levels,
  log_levels = getOption("yog.log_levels")
){
  if (!is.numeric(levels))
    stop("Expected numeric 'levels'")

  res <- names(log_levels)[match(levels, log_levels)]
  res[is.na(levels)] <- "all"
  res[levels == 0] <- "off"
  names(res) <- levels

  if (anyNA(res))
    warning("Some 'levels' were not valid numeric log levels, coercing to NA")

  res
}




#' @rdname label_levels
#' @param log_levels
unlabel_levels = function(
  labels,
  log_levels = getOption("yog.log_levels")
){
  if (!is.character(labels))
    stop("Expected character 'labels'")

  res <- log_levels[match(labels, names(log_levels))]

  if (anyNA(labels) || any(!labels %in% c(names(log_levels), "all", "off")))
    warning("Some 'labels' were not valid character log levels, coercing to NA")

  res[labels == "off"] <- 0L
  names(res) <- labels
  res
}
