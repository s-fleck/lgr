#' Print or Format Logging Data
#'
#' @param x a [LogEvent] or [lgr_data] Object
#' @param timestamp_fmt see [format.POSIXct()]
#' @param fmt A `character` scalar that may contain any of the tokens listed
#'   bellow in the section Format Tokens.
#' @param colors A `list` of `functions` that will be used to color the
#'   log levels (likely from [crayon::crayon]).
#' @param log_levels a named `integer` vector of log levels.
#' @param pad_levels `right`, `left` or `NULL`. Whether or not to pad the log
#'   level names to the same width on the left or right side, or not at all.
#' @param ... ignored
#'
#' @section Format Tokens:
#' \describe{
#'   \item{`%t`}{The timestamp of the message, formatted according to
#'     `timestamp_fmt`)}
#'   \item{`%l`}{the log level, lowercase `character` representation}
#'   \item{`%L`}{the log level, uppercase `character` representation}
#'   \item{`%k`}{the log level, first letter of lowercase `character` representation}
#'   \item{`%K`}{the log level, first letter of uppercase `character` representation}
#'   \item{`%n`}{the log level, `integer` representation}
#'   \item{`%p`}{the PID (process ID). Useful when logging code that uses
#'       multiple threads.}
#'   \item{`%c`}{the calling function}
#'   \item{`%m`}{the log message}
#'   \item{`%f`}{all custom fields of `x` in a pseudo-JSON like format that is
#'     optimized for human readability and console output}
#'   \item{`%j`}{all custom fields of `x` in proper JSON. This requires that you
#'     have **jsonlite** installed and does not support colors as opposed to
#'     `%f`
#'   }
#' }
#'
#' @return `x` for `print()` and a `character` scalar for `format()`
#' @export
#'
#' @examples
#' # standard fields can be printed using special tokens
#' x <- LogEvent$new(
#'   level = 300, msg = "a test event", caller = "testfun()", logger = lgr
#' )
#' print(x)
#' print(x, fmt = c("%t (%p) %c: %n - %m"))
#' print(x, colors = NULL)
#'
#' # custom values
#' y <- LogEvent$new(
#'   level = 300, msg = "a gps track", logger = lgr,
#'   waypoints = 10, location = "Austria"
#' )
#'
#' # default output with %f
#' print(y)
#'
#' # proper JSON output with %j
#' if (requireNamespace("jsonlite")){
#' print(y, fmt = "%L [%t] %m  %j")
#' }
#'
print.LogEvent <- function(
  x,
  fmt = "%L [%t] %m  %f",
  timestamp_fmt = "%Y-%m-%d %H:%M:%S",
  colors = getOption("lgr.colors"),
  log_levels = getOption("lgr.log_levels"),
  pad_levels = "right",
  ...
){
  cat(format(
    x,
    fmt = fmt,
    timestamp_fmt = timestamp_fmt,
    colors = colors,
    log_levels = log_levels,
    pad_levels = pad_levels
  ), sep = "\n")
  invisible(x)
}




#' @rdname print.LogEvent
#' @export
format.LogEvent <- function(
  x,
  fmt = "%L [%t] %m  %f",
  timestamp_fmt = "%Y-%m-%d %H:%M:%S",
  colors = NULL,
  log_levels = getOption("lgr.log_levels"),
  pad_levels = "right",
  ...
){
  stopifnot(
    is_scalar_character(fmt),
    is_scalar_character(timestamp_fmt),
    is_scalar_character(pad_levels) || is.null(pad_levels)
  )

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
    valid_tokens = paste0(
      "%",
      c("t", "p", "c", "m", "l", "L", "n", "f", "j", "k", "K", "g"))
  )

  # format
  len  <- length(tokens)
  res  <- vector("list", length(tokens))

  for(i in seq_len(len)){
    res[[i]] <- switch(
      tokens[[i]],
      "%n" = colorize_levels(x$level, colors),
      "%l" = colorize_levels(lvls, colors),
      "%L" = colorize_levels(toupper(lvls), colors),
      "%k" = colorize_levels(substr(lvls, 1, 1), colors),
      "%K" = colorize_levels(substr(toupper(lvls), 1, 1), colors),
      "%t" = format(get("timestamp", envir = x), format = timestamp_fmt),
      "%m" = get("msg", envir = x),
      "%c" = get("caller", envir = x),
      "%g" = get("logger", envir = x),
      "%p" = Sys.getpid(),
      "%f" = format_custom_fields(get_custom_fields(x), color = length(colors)),
      "%j" = jsonlite::toJSON(get_custom_fields(x), auto_unbox = TRUE),
      tokens[[i]]
    )
  }

  do.call(paste0, res)
}




get_custom_fields <- function(x){
  x$values[!names(x$values) %in% DEFAULT_FIELDS]
}




format_custom_fields <- function(
  x,
  color = TRUE
){
  if (!length(x)) return("")

  max_len <- max(256 / length(x) - sum(nchar(names(x))), 16)

  braces   <- c("{", "}")
  brackets <- c("[", "]")
  colon    <- ": "
  comma    <- ", "
  dots     <- ".."

  if (!color){
    style_subtle <- identity
    style_accent <- identity
  } else {
    braces   <- style_subtle(braces)
    brackets <- style_subtle(brackets)
    colon    <- style_subtle(colon)
    comma    <- style_subtle(comma)
    dots     <- style_subtle("..")
  }

  res <- lapply(
    x,
    preview_object,
    width = max_len, brackets = brackets, dots = dots, quotes = c("", "")
  )

  paste0(
    braces[[1]],
    paste(
      style_accent(names(res)), colon, res,
      sep = "",
      collapse  = comma
    ),
    braces[[2]]
  )
}




tokenize_format <- function(
  x,
  valid_tokens = NULL
){
  pos <- unlist(gregexpr("%.", x))

  if (identical(pos, -1L))
    return(x)
  pos <- sort(unique(c(1L, pos, pos + 2L, nchar(x) + 1L)))
  res <- vector("character", length(x))

  for(i in seq_len(length(pos) - 1L)) {
    res[[i]] <- substr(x, pos[[i]], pos[[i + 1]] - 1L)
  }

  if (!is.null(valid_tokens)){
    placeholders <- grep("%", res, value = TRUE, fixed = TRUE)
    assert(
      all(placeholders %in% valid_tokens),
      "'format' contains unrecognised format specifications: ",
      paste(sort(setdiff(placeholders, valid_tokens)), collapse = ", ")
    )
  }

  res
}



# convert a data.frame as returned for example by AppenderDt$dt to a list of
# LogEvent like environments. Useful for printing.
as_LogEvent_list.data.frame <- function(x, na.rm = TRUE){
  lapply(
    seq_len(nrow(x)),
    function(i){
      dd <- as.list(x[i, !names(x) %in% c(".id", ".custom")])

      if (na.rm){
        for (j in rev(seq_along(dd))) if (is.na(dd[[j]])) dd[[j]] <- NULL
      }

      r <- as.environment(c(dd, x[i, ][[".custom"]][[1]]))
      r[["values"]] <- rev(as.list(r))
      r
    }
  )
}
