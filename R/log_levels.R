as_log_levels <- function(x){
  assert(is_integerish(x) && identical(length(names(x)), length(x)))
  assert(all_are_distinct(x))
  assert(all_are_distinct(names(x)))
  assert(
    !anyNA(x) & all (x > 0),
    "Log levels must be positive integers. The log levels `0` (off) and `NA` ",
    "(all) are reserved."
  )
  x <- setNames(as.integer(x), names(x))
  structure(sort(x), class = c("log_levels", "integer"))
}




DEFAULT_LOG_LEVELS <- c("fatal", "error", "warn", "info", "debug", "trace")




# manage options ----------------------------------------------------------

#' Manage Log Levels
#'
#' Display, add and remove character labels for log levels.
#'
#' @return a named `character` vector of the globally available log levels
#'   (`add_log_levels()` and `remove_log_levels()` return invisibly).
#' @export
#'
#'
#' @section Default Log Levels:
#'
#' lgr comes with the following predefined log levels that are identical to
#' the log levels of log4j.
#'
#'\tabular{rll}{
#'  Level \tab Name  \tab Description \cr
#'    `0` \tab off   \tab A log level of 0/off tells a Logger or Appender to suspend all logging \cr
#'  `100` \tab fatal \tab Critical error that leads to program abort. Should always indicate a `stop()` or similar \cr
#'  `200` \tab error \tab A severe error that does not trigger program abort\cr
#'  `300` \tab warn  \tab A potentially harmful situation, like `warning()`\cr
#'  `400` \tab info  \tab An informational message on the progress of the application\cr
#'  `500` \tab debug \tab Finer grained informational messages that are mostly useful for debugging\cr
#'  `600` \tab trace \tab An even finer grained message than debug\cr
#'   `NA` \tab all   \tab A log level of NA/all tells a Logger or Appender to process all log events
#' }
#' @aliases log_levels log_level
#' @examples
#' get_log_levels()
#' add_log_levels(c(errorish = 250))
#' get_log_levels()
#' remove_log_levels("errorish")
#' get_log_levels()
get_log_levels <- function(){
  getOption("lgr.log_levels")
}




#' @param levels a named `character` vector (see examples)
#' @rdname get_log_levels
#' @export
add_log_levels <- function(
  levels
){
  levels_cur <- getOption("lgr.log_levels")
  assert(
    !is.null(levels_cur),
    "lgr.log_levels option is not set. something is very wrong with lgr, please file a bug report"
  )
  assert(
    is_integerish(levels),
    "`levels` must be a named integer vector"
  )
  assert(length(names(levels)) && all_are_distinct(names(levels)))

  levels_new <- setNames(as.integer(levels), names(levels))
  levels_upd <- levels_new[levels_new %in% levels_cur]

  if (length(levels_upd)){
    message(
      "Replacing existing log levels '",
      fmt_log_levels(levels_new[levels_new %in% levels_cur]),
      "' with '",
      fmt_log_levels(levels_upd), "'"
    )
  }

  res <- as_log_levels(named_union(levels_cur, levels_new))

  options(lgr.log_levels = res)
  invisible(get_log_levels())
}




#' union for named vectors. names of `y` override names of `x`
#' @noRd
named_union <- function(x, y){
  assert(identical(length(names(x)), length(x)))
  assert(identical(length(names(y)), length(y)))

  r <- union(x, y)
  names(r)[r %in% x] <- names(x[x %in% r])
  names(r)[r %in% y] <- names(y[y %in% r])
  r
}




#' @param level_names a `character` vector of the names of the levels to remove
#' @rdname get_log_levels
#' @export
remove_log_levels <- function(
  level_names
){
  assert(is.character(level_names))
  assert(
    !any(DEFAULT_LOG_LEVELS %in% level_names),
    "Cannot remove default log levels"
  )
  current_lvls <- getOption("lgr.log_levels")
  assert(all(level_names %in% names(current_lvls)))
  res <- current_lvls[!names(current_lvls) %in% level_names]
  res <- as_log_levels(res)
  options(lgr.log_levels = res)
  invisible(get_log_levels())
}




# format ------------------------------------------------------------------

fmt_log_levels <- function(
  x
){
  paste0(names(sort(x)), " (", sort(x), ")", collapse = ", ")
}




#' Colorize Levels
#'
#' @param x `numeric` or `character` levels to be colored. Unlike in many other
#'   functions in lgr, `character` levels are *not* case sensitive in this
#'   function and leading/trailing whitespace is ignored to make it more
#'   comfortable to use `colorize_levels()` inside formatting functions.
#' @inheritParams format.LogEvent
#'
#' @return a `character` vector wit color ANSI codes
#' @family formatting utils
#' @export
#'
#' @examples
#' cat(colorize_levels(c(100, 200)))
#' cat(colorize_levels(c("trace", "warn ", "DEBUG")))
colorize_levels <- function(
  x,
  colors = getOption("lgr.colors", NULL)
){
  if (length(x) && length(colors)){
    if (is.character(x))
      dd <- standardize_log_levels(trimws(tolower(x)))
    else
      dd <- standardize_log_levels(x)

    cn <- standardize_log_levels( tolower(names(colors)))
    for (i in seq_along(colors)){
      sel <- dd == cn[[i]]
      x[sel] <- colors[[i]](x[sel])
    }
  }

  x
}




# standardize ------------------------------------------------------------

#' Standardize User-Input Log Levels to Their Integer Representation
#'
#' @param x a `character` or `integer` scalar, or vector for
#'   standardize_log_levels
#' @param log_levels A named character vector of valid log levels
#'
#' @return An unnamed `integer` vector
#'
#' @noRd
standardize_threshold <- function(
  x,
  log_levels = c(getOption("lgr.log_levels"), c("all" = NA_integer_, "off" = 0L)),
  allow_null = FALSE
){
  assert(is_scalar(x), "A threshold must be a scalar (a vector of length 1)" )

  if (is.na(x)){
    return(NA_integer_)
  }

  if (is_integerish(x) && x >= 0){
    return(as.integer(x))
  }

  if (is.character(x) && (x %in% names(log_levels)) ){
    return(unname(log_levels[match(x, names(log_levels))]))
  }

  stop(error_msg_log_levels(deparse(substitute(x)), log_levels))
}




is_threshold <- function(x){
  tryCatch(
    {standardize_threshold(x); TRUE},
    error = function(...) FALSE
  )
}




standardize_log_level <- function(
  x,
  log_levels = getOption("lgr.log_levels")
){
  assert(is_scalar(x), "'", deparse(substitute(x)), "' must be a scalar log level")

  if (is_integerish(x) && x > 0){
    return(as.integer(x))
  }

  if (is.character(x) && (x %in% names(log_levels)) ){
    return(unname(log_levels[match(x, names(log_levels))]))
  }

  stop(error_msg_log_levels(deparse(substitute(x)), log_levels))
}




is_log_level <- function(x){
  tryCatch(
    {standardize_log_level(x); TRUE},
    error = function(...) FALSE
  )
}




standardize_log_levels <- function(
  x,
  log_levels = getOption("lgr.log_levels")
){

  if (is_integerish(x) && all(x > 0)){
    return(as.integer(x))
  }

  if (is.character(x) && all(x %in% names(log_levels)) ){
    return(unname(log_levels[match(x, names(log_levels))]))
  }

  stop(error_msg_log_levels(deparse(substitute(x)), log_levels))
}




is_log_levels <- function(x){
  tryCatch(
    {standardize_log_levels(x); TRUE},
    error = function(...) FALSE
  )
}




error_msg_log_levels <- function(varname, log_levels){
  ll_text <-
    paste(sprintf("%s (%s)", names(log_levels), log_levels), collapse = ", ")

  paste0(
    "'", varname, "' must either the numeric or character representation",
    "of one of the following log levels: ", ll_text
  )
}




# label/unlabel -----------------------------------------------------------

#' Label/Unlabel Log Levels
#'
#' @param levels an `integer` vector of log levels
#' @param labels a `character` vector of log level labels. Please note that
#'   log levels are lowercase by default, even if many appenders print them
#'   in uppercase.
#' @param log_levels a named `integer` vector, should usually not be set
#'   manually.
#'
#' @return a `character` vector for `label_levels()` and an integer vector for
#'   `unlabel_levels`
#'
#' @seealso [get_log_levels()]
#' @family formatting utils
#' @export
#' @examples
#' x <- label_levels(c(seq(0, 600, by = 100), NA))
#' print(x)
#' unlabel_levels(x)

label_levels <- function(
  levels,
  log_levels = getOption("lgr.log_levels")
){
  if (!is.numeric(levels))
    stop("Expected numeric 'levels'")

  res <- names(log_levels)[match(levels, log_levels)]
  res[is.na(levels)] <- "all"
  res[levels == 0]   <- "off"
  names(res)         <- levels

  if (anyNA(res))
    warning("Some `levels` were not valid numeric log levels, coercing to `NA`")

  res
}




#' @rdname label_levels
#' @export
unlabel_levels <- function(
  labels,
  log_levels = getOption("lgr.log_levels")
){
  if (!is.character(labels))
    stop("Expected character 'labels'")

  res             <- log_levels[match(labels, names(log_levels))]
  if (!is.null(names(labels)) && any(is.na(res))){
    res[is.na(res)] <- as.integer(names(labels)[is.na(res)])
  }

  if (anyNA(labels) || any(!labels %in% c(names(log_levels), "all", "off")))
    warning(
      "Some `labels` were not valid character log levels. Please",
      "consider adding them to the global log levels with `?add_log_levels`"
    )

  res[labels == "off"] <- 0L
  names(res) <- labels
  res
}
