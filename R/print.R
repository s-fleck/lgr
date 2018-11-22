


#' Format Logging Data
#'
#' @param x a [LogEvent] or [yog_data] Object
#' @param timestamp_fmt see [format.POSIXct]
#' @param fmt A format string that may contain the following tokens:
#'
#' \describe{
#'   \item{`%t`} A timestamp (see also `timestamp_fmt`)
#'   \item{`%l`} the log level
#'   \item{`%L`} the log level (uppercase)
#'   \item{`%n`} the log level (numeric)
#'   \item{`%u`} the current user
#'   \item{`%p`} the PID (process ID)
#'   \item{`%c`} the calling function
#'   \item{`%m`} the log message
#' }
#'
#'
#' @param colors A `list` of `functions` that will be used to color the
#'   log levels (likely from [crayon] or [colt]).
#' @param log_levels a named `integer` vector of log levels.
#' @param pad_levels `right`, `left` or `NULL`. Whether or not to pad the log
#'   level names to the same width on the left or right side, or not at all.
#' @param user The user
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
format.LogEvent <- function(
  x,
  fmt = "%L [%t] %m",
  timestamp_fmt = "%Y-%m-%d %H:%M:%S",
  colors = NULL,
  log_levels = getOption("yog.log_levels"),
  pad_levels = "right",
  user = get_user(),
  ...
){
  stopifnot(
    is_scalar_character(fmt),
    is_scalar_character(timestamp_fmt),
    is_scalar_character(pad_levels) || is.null(pad_levels)
  )


  # degenerate cases
  if (identical(nrow(x), 0L))  return("[empty log]")


  # init
  lvls <- label_levels(x$level, log_levels = log_levels)
  lvls[is.na(lvls)] <- x$level[is.na(lvls)]

  if (!is.null(pad_levels)){
    nchar_max <- max(nchar(names(log_levels)))
    diff <- nchar_max - nchar(lvls)
    pad <- vapply(diff, function(i) paste(rep.int(" ", i), collapse = ""), character(1))

    if (pad_levels == "right"){
      lvls <- paste0(lvls, pad)
    } else {
      lvls <- paste0(pad, lvls)
    }

  } else {
    lvls <- x$level
  }

  # tokenize
  tokens <- tokenize_format(
    fmt,
    valid_tokens = c("%t", "%u", "%p", "%c", "%m", "%l", "%L", "%n")
  )

  # format
  len  <- length(tokens)
  res  <- vector("list", length(tokens))
  for(i in seq_len(len)){
    res[[i]] <- switch(
      tokens[[i]],
      "%n" = colorize_levels(x$level, x$level, colors, unlabel_levels(names(colors), log_levels = log_levels  )),
      "%l" = colorize_levels(lvls, x$level, colors, unlabel_levels(names(colors), log_levels = log_levels  )),
      "%L" = colorize_levels(toupper(lvls), x$level, colors, unlabel_levels(names(colors), log_levels = log_levels )),
      "%t" = format(x$timestamp, format = timestamp_fmt),
      "%m" = x$msg,
      "%c" = x$caller %||% "(unknown function)",
      "%u" = user,
      "%p" = Sys.getpid(),
      tokens[[i]]
    )
  }
  res <- do.call(paste0, res)

  res
}




#' @export
format.yog_data <- format.LogEvent


tokenize_format <- function(
  x,
  valid_tokens = NULL
){
  pos <- unlist(gregexpr("%.", x))

  if (identical(pos, -1L))
    return(x)
  pos <- sort(unique(c(1L, pos, pos + 2L, nchar(x) + 1L)))
  res <- vector("character", length(x))
  begin <- 1L
  for(i in seq_len(length(pos) -1L)) {
    res[[i]] <- substr(x, pos[[i]], pos[[i + 1]] - 1L)
  }

  if (!is.null(valid_tokens)){
    placeholders <- grep("%", res, value = TRUE)
    assert(
      all(placeholders %in% valid_tokens),
      "'format' contains unrecognised format specifications: ",
      paste(sort(setdiff(placeholders, valid_tokens)), collapse = ", ")
    )
  }

  res
}




#' @param x levels to be colored
#' @param num_levels numeric version of x (to match against numeric color levels)
#' @param colors named list of coloring functions. the names should be the levels in x
#' @param num_levels_colors numeric version of the names of `colors` to be matched
#'   against num_levels
#'
#' @return a `character` vector wit color ansi codes
#' @nord
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




#' Title
#'
#' @param x
#' @param log_levels
#'
#' @return
#' @export
#'
#' @examples
label_levels = function(
  x,
  log_levels = getOption("yog.log_levels")
){
  if (!is.numeric(x)) stop("Expected numeric 'x'")
  res <- names(log_levels)[match(x, log_levels)]
  res[is.na(x)] <- "all"
  res[x == 0] <- "off"
  res
}




#' Title
#'
#' @param x
#' @param log_levels
#'
#' @return
#' @export
#'
#' @examples
unlabel_levels = function(
  x,
  log_levels = getOption("yog.log_levels")
){
  if (!is.character(x)) stop("Expected character 'x'")
  log_levels[match(x, names(log_levels))]
}




pad_left <- function(
  x,
  width = max(nchar(paste(x))),
  pad = " "
){
  diff <- width - nchar(paste(x))
  padding <-
    vapply(diff, function(i) paste(rep.int(pad, i), collapse = ""), character(1))
  paste0(padding, x)
}




pad_right <- function(
  x,
  width = max(nchar(paste(x))),
  pad = " "
){
  diff <- width - nchar(paste(x))
  padding <-
    vapply(diff, function(i) paste(rep.int(pad, i), collapse = ""), character(1))
  paste0(x, padding)
}




#' Single Row Summary of yog objects
#' @return a 1 row `data.frame` with columns `name`, `threshold`, `comment`
#' @noRd
srs <- function(
  x
){
  res <- data.frame(
    name = class(x)[[1]],
    threshold = x$threshold,
    comment = "",
    stringsAsFactors = FALSE
  )

  if (inherits(x, "AppenderFile"))
    res$comment <- style_subtle(paste(" ->", x$file))

  res
}




#' Single Line Summary
#'
#' @param x
#'
sls <- function(
  x,
  colors = FALSE
){
  if (inherits(x, "Appender"))
    return(paste(as.character(as.list(srs(x))), collapse = " "))

  if (is.function(x))
    return(trimws(paste0(format(x)[[1]])))

  if (inherits(x, "log_levels"))
    return(paste0(names(x), " (", x, ")", collapse = ", "))

  if (is.atomic(x))
    return(ptrunc_col(x, width = 64))

  class_fmt(x)
}




fmt_threshold <- function(
  x,
  log_levels = getOption("yog.log_levels")
){
  paste0(label_levels(x, log_levels), " (", x, ")")
}




#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
format.ancestry <- function(x, ...){
  paste(x, collapse = " -> ")
}




print.ancestry <- function(x, ...){
  cat(format(x))
}




summary.Logger = function(x){

}




style_accent <- colt::clt_chr_accent




style_subtle <- colt::clt_chr_subtle
