#' Colorize Levels
#'
#' @param x `numeric` or `character` levels to be colored. Unlike in many other
#'   functions in lgr, `character` levels are *not* case sensitive in this
#'   function and leading/trailing whitespace is ignored to make it more
#'   comfortable to use `colorize_levels()` inside formatting functions.
#' @inheritParams format.LogEvent
#'
#' @return a `character` vector wit color ansi codes
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




#' Label/Unlabel Log Levels
#'
#' @param levels an `integer` vector of log levels
#' @param labels a `character` vectpr of log level labels. Please note that
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
#'
#' x <- label_levels(c(seq(0, 600, by = 100), NA))
#' print(x)
#' unlabel_levels(x)
#'
label_levels = function(
  levels,
  log_levels = getOption("lgr.log_levels")
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
#' @export
unlabel_levels = function(
  labels,
  log_levels = getOption("lgr.log_levels")
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




#' Pad Character Vectors
#'
#' @param x a `character` vector
#' @param width `integer` scalar. target string width
#' @param pad `character` scalar. the symbol to pad with
#'
#' @export pad_right
#' @name pad_right
#'
#' @examples
#' pad_left("foo", 5)
#' pad_right("foo", 5, ".")
#' pad_left(c("foo", "foooooo"), pad = ".")
NULL




#' @export pad_left
#' @rdname pad_right
#' @name pad_left
NULL
