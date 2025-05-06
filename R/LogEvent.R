# LogEvent ----------------------------------------------------------------

#' LogEvents - The atomic unit of logging
#'
#' @description
#' A `LogEvent` is a single unit of data that should be logged. `LogEvents` are
#' usually created by a [Logger], and then processed by one more [Appenders].
#' They do not need to be instantiated manually except for testing and
#' experimentation; however, if you plan on writing your own Appenders or
#' Layouts you need to understand LogEvents.
#'
#' @seealso [as.data.frame.LogEvent()]
#' @family docs relevant for extending lgr
#' @aliases LogEvents
#' @examples
#' lg <- get_logger("test")
#' lg$error("foo bar")
#'
#' # The last LogEvent produced by a Logger is stored in its `last_event` field
#' lg$last_event  # formatted console output
#' lg$last_event$values  # values stored in the event
#'
#' # Also contains the Logger that created it as .logger
#' lg$last_event$logger
#' # equivalent to
#' lg$last_event$.logger$name
#'
#' # This is really a reference to the complete Logger, so the following is
#' # possible (though nonsensical)
#' lg$last_event$.logger$last_event$msg
#' identical(lg, lg$last_event$.logger)
#' lg$config(NULL)  # reset logger config
#' @export
LogEvent <- R6::R6Class(
  "LogEvent",
  lock_objects = FALSE,
  public = list(

  #' @description
  #' The arguments to `LogEvent$new()` directly translate to the fields stored
  #' in the `LogEvent`. Usually these values will be scalars, but (except for
  #' `"logger"`) they can also be vectors if they are all of the same length (or
  #' scalars that will be recycled). In this case the event will be treated by
  #' the [Appenders] and [Layouts] as if several separate events.
  #'
  #' @param ...  All named arguments in `...` will be added to the LogEvent
  #'   as **custom fields**. You can store arbitrary \R objects in LogEvents
  #'   this way, but not all Appenders will support them. See [AppenderJson] for
  #' @param logger,level,timestamp,caller,msg see **Public fields**.
    initialize = function(
      logger,
      level = 400,
      timestamp = Sys.time(),
      caller = NA,
      msg = NA,
      .rawMsg = msg,
      ...
    ){
      assert(inherits(logger, "Logger"), "Logger must be a <Logger> object, not a ", class_fmt(logger))

      # assign has less overhead than [[ and event creation needs to be as fast
      # as possible
      assign(".logger", logger, self)
      assign("level", level, self)
      assign("timestamp", timestamp, self)
      assign("caller", caller, self)
      assign("msg", msg, self)
      assign(".rawMsg", .rawMsg, self)

      # custom values
      if (!missing(...)){
        dots <- list(...)
        assert(identical(length(names(dots)), length(dots)))
        # the rev() ensures that the values get added int eh same order as
        # the user entered them
        for (nm in rev(names(dots))){
          assign(nm, dots[[nm]], self)
        }
      }
    },

    #' @field level `integer`. The [log_level] / priority of the LogEvent. Use the
    #' active binding `level_name` to get the `character` representation
    #' instead.
    level = NULL,

    #' @field timestamp [`POSIXct`][base::POSIXct]. The time when then the
    #'   LogEvent was created.
    timestamp = NULL,

    #' @field caller `character`. The name of the calling function.
    caller = NULL,

    #' @field msg `character`. The log message.
    msg = NULL,

    #' @field .logger [Logger]. A reference to the Logger that created the
    #' event (equivalent to `get_logger(event$logger)`).
    .logger = NULL,

    #' @field .rawMsg `character`. The raw log message without string
    #'   interpolation.
    .rawMsg = NULL
  ),

  active = list(

    #' @field values `list`. All values stored in the `LogEvent`, including
    #' all *custom fields*, but not including `event$.logger` and `event$.rawMsg`.
    values = function(){
      fixed_vals   <- c("level", "timestamp", "logger", "caller", "msg")
      custom_vals <- setdiff(
        names(get(".__enclos_env__", self)[["self"]]),
        c(".__enclos_env__", "level_name", "initialize", "clone", "values", ".rawMsg",
          ".logger")
      )
      valnames <- union(fixed_vals, custom_vals) # to enforce order of fixed_vals
      mget(valnames, envir = self)
    },

    #' @field level_name `character`. The [log_level] / priority of the LogEvent labelled
    #' according to `getOption("lgr.log_levels")`
    level_name = function(){
      label_levels(get("level", envir = self))
    },

    #' @field logger `character` scalar. The name of the Logger that
    #' created this event, equivalent to `event$.logger$name`)
    logger = function(){
      get("name", envir = get(".logger", envir = self))
    }
  )
)




# coercion ---------------------------------------------------

#' Coerce objects to LogEvent
#'
#' Smartly coerce \R objects that look like LogEvents to LogEvents. Mainly
#' useful for developing Appenders.
#'
#' **Note**: `as_LogEvent.data.frame()` only supports single-row `data.frames`
#'
#' @param x any supported \R object
#' @param ... currently ignored
#'
#' @return a [LogEvent]
#' @family docs relevant for extending lgr
#' @export
as_LogEvent <- function(x, ...){
  UseMethod("as_LogEvent")
}




#' @rdname as_LogEvent
#' @export
as_LogEvent.list <- function(x, ...){

  if (is.null(x[["logger"]])){
    x[["logger"]] <- get_logger()
  } else if (is.character(x[["logger"]])){
    x[["logger"]] <- get_logger(x[["logger"]])
  }

  # smartly rename timestamp fields from ElasticSearch/Logstash
  if (!"timestamp" %in% names(x) && "@timestamp" %in% names(x)){
    names(x)[names(x) == "@timestamp"] <- "timestamp"
  }

  x[["level"]] <- standardize_log_level(x[["level"]])

  do.call(LogEvent$new, x)
}




#' @rdname as_LogEvent
#' @export
as_LogEvent.data.frame <- function(
  x,
  ...
){
  assert(
    identical(nrow(x), 1L),
    "`as_LogEvent()` only supports single-row data.frames. Try `as_event_list()` instead"
  )

  as_LogEvent(unclass(x))
}




#' Coerce LogEvents to Data Frames
#'
#' Coerce LogEvents to `data.frames`, [`data.tables`][data.table::data.table],
#' or [`tibbles`][tibble::tibble].
#'
#' @inheritParams base::as.data.frame
#' @param stringsAsFactors `logical` scalar: should `character` vectors be
#'   converted to factors? Defaults to `FALSE` (as opposed to
#'   [base::as.data.frame()]) and is only included for compatibility.
#' @param ... passed on to `data.frame()`
#' @param optional currently ignored and only included for compatibility.
#' @param box_if a `function` that returns `TRUE` or `FALSE` to determine
#'   which values are to be boxed (i.e. placed as single elements in a list
#'   column). See example
#' @param cols_expand `character` vector. Columns to *not* box (even if
#'   `box_if()` returns `TRUE`). Vectors in these columns will result in multiple
#'   rows in the result (rather than a single list-column row). This defaults to
#'   `"msg"` for vectorized logging over the log message.
#' @export
#' @seealso [data.table::data.table], [tibble::tibble]
#'
#' @examples
#' lg <- get_logger("test")
#' lg$info("lorem ipsum")
#' as.data.frame(lg$last_event)
#'
#' lg$info("LogEvents can store any custom log values", df = iris)
#' as.data.frame(lg$last_event)
#' head(as.data.frame(lg$last_event)$df[[1]])
#'
#' # how boxing works
#'
#' # by default non-scalars are boxed
#' lg$info("letters", letters = letters)
#' as.data.frame(lg$last_event)
#'
#' # this behaviour can be modified by supplying a custom boxing function
#' as.data.frame(lg$last_event, box_if = function(.) FALSE)
#' as.data.frame(lg$last_event, cols_expand = "letters")
#'
#' # The `msg` argument of a log event is always vectorized
#' lg$info(c("a vectorized", "log message"))
#' as.data.frame(lg$last_event)
#'
#' lg$config(NULL)
as.data.frame.LogEvent <- function(
  x,
  row.names = NULL,
  optional = FALSE,
  stringsAsFactors = FALSE,
  ...,
  box_if = function(.) !(is.atomic(.) && identical(length(.), 1L)),
  cols_expand  = NULL
){
  assert(is.null(cols_expand) || is.character(cols_expand))
  boxer <- function(.) I(list(.))
  values <- do_box_if(x$values, box_if, except = cols_expand, boxer = boxer)
  msg_len <- length(x[["msg"]])
  cols_expand <- c("msg", cols_expand)
  # as.data.frame requires manual recycling

  if (msg_len > 1){
    for (i in seq_along(values)){
      if (identical(length(values[[i]]), 1L))
        values[[i]] <- rep(values[[i]], msg_len)
    }
    values <- do_box_if(values, box_if, except = cols_expand, boxer = I)
  }

  do.call(
    data.frame,
    c(values,
      stringsAsFactors = stringsAsFactors,
      row.names = row.names,
      ...
    )
  )
}



#' @rdname as.data.frame.LogEvent
as.data.table.LogEvent <- function(
  x,
  ...,
  box_if = function(.) !(is.atomic(.) && identical(length(.), 1L)),
  cols_expand  = "msg"
){
  values <- do_box_if(x$values, box_if, cols_expand)
  data.table::as.data.table(values)
}




#' @rdname as.data.frame.LogEvent
as_tibble.LogEvent <- function(
  x,
  ...,
  box_if = function(.) !(is.atomic(.) && identical(length(.), 1L)),
  cols_expand  = "msg"
){
  values <- do_box_if(x$values, box_if, cols_expand)
  tibble::as_tibble(values)
}




# printing ---------------------------------------------------

#' Print or Format Logging Data
#'
#' @param x a [LogEvent]
#' @param timestamp_fmt see [format.POSIXct()]
#' @param fmt A `character` scalar that may contain any of the tokens listed
#'   bellow in the section Format Tokens.
#' @param colors A `list` of `functions` that will be used to color the
#'   log levels (likely from [crayon::crayon]).
#' @inheritParams standardize_threshold
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
#'   \item{`%g`}{the name of the logger}
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
      "%k" = colorize_levels(lvls, colors, transform = function(.) strtrim(., 1)),
      "%K" = colorize_levels(lvls, colors, transform = function(.) toupper(strtrim(., 1))),
      "%t" = format(get("timestamp", envir = x), format = timestamp_fmt),
      "%m" = get("msg", envir = x),
      "%c" = get("caller", envir = x),
      "%g" = get("logger", envir = x),
      "%p" = Sys.getpid(),
      "%f" = format_custom_fields(get_custom_fields(x), color = length(colors)),
      "%j" = format_custom_fields_json(get_custom_fields(x)),
      tokens[[i]]
    )
  }

  sub("[ \t\r]*$", "", do.call(paste0, res))
}



format_custom_fields_json <- function(x){
  if (length(x)){
    jsonlite::toJSON(x, auto_unbox = TRUE)
  } else {
    ""
  }
}



#' Convert a LogEvent to a character string
#'
#' @param x a [LogEvent]
#' @param ... ignored
#'
#' @return a `character` scalar
#' @export
#'
#' @examples
#' toString(LogEvent$new(logger = lgr::lgr))
toString.LogEvent <- function(x, ...){
  paste(
    paste0("$", names(x$values), ": ", vapply(x$values, function(.) string_repr(.), character(1L))),
    collapse = ", "
  )
}




# utils -------------------------------------------------------------------

do_box_if <- function(x, predicate, except, boxer = list){
  sel <-
    vapply(x, predicate, TRUE, USE.NAMES = FALSE)

  if (length(except)){
    sel[(names(x) %in% except)] <- FALSE
  }

  for (i in seq_along(x))
    if (sel[[i]])
      x[[i]] <- boxer(x[[i]])

  x
}




get_custom_fields <- function(x){
  x$values[!names(x$values) %in% DEFAULT_FIELDS]
}




format_custom_fields <- function(
  x,
  color = TRUE
){
  if (!length(x)) return("")

  max_len <- floor(max(512 / length(x) - sum(nchar(names(x))), 16))

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
    colon    <- style_subtle(colon)
    comma    <- style_subtle(comma)
  }

  res <- lapply(
    x,
    string_repr,
    width = max_len
  )

  paste0(
    braces[[1L]],
    paste(
      style_accent(names(res)), colon, res,
      sep = "",
      collapse = comma
    ),
    braces[[2L]]
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
    res[[i]] <- substr(x, pos[[i]], pos[[i + 1L]] - 1L)
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




# globals --------------------------------------------------------

DEFAULT_FIELDS <- c("level", "timestamp", "logger", "caller", "msg")
