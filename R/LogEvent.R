#' Events - The Atomic Unit of Logging
#'
#' A `LogEvent` is a single unit of data that should be logged. `LogEvents` are
#' usually created by a [Logger], and then processed by [Appenders].
#'
#' @eval r6_usage(LogEvent)
#'
#' @section Creating LogEvents / Fields:
#'
#' The arguments to `LogEvent$new()` directly translate to the fields stored in
#' the LogEvent:
#'
#' \describe{
#'   \item{`level`}{`integer`: the [log_level] / priority of the LogEvent}
#'
#'   \item{`timestamp`}{[`POSIXct`][base::POSIXct] the time when then the
#'   LogEvent was created}
#'
#'   \item{`caller`}{`character`. The name of the calling function}
#'
#'   \item{`msg`}{`character`. A message}
#'
#'   \item{`logger`}{`character` scalar. Name of the Logger that created the
#'     event (`.logger$full_name`)
#'   }
#'
#'   \item{`user`}{`character` scalar. User as set for the Logger
#'     that created this event (`.logger$user`)
#'   }
#'
#'   \item{`.logger`}{a `Logger`. A reference to the Logger that created the
#'     event
#'   }
#'
#'   \item{`...`}{All named arguments in `...` will be added to the LogEvent
#'   as **custom fields**. You can store arbitrary \R objects in LogEvents
#'   this way, but not all Appenders will support them.
#'   See [AppenderJson] for
#'   an Appender that supports custom fields quite naturally.}
#' }
#'
#'
#' Usually the above values will be scalars, but (except for `"logger"`) they
#' can also be vectors if they are all of the same length (or scalars that will
#' be recycled). In this case the event will be treated by the [Appenders] and
#' [Layouts] as if several separate events.
#'
#' @section Active Bindings:
#'
#' LogEvents contain some some active bindings that make it easier to retrieve
#' commonly used values.
#'
#' \describe{
#'   \item{`level_name`}{`character`: the [log_level] / priority of the
#'     LogEvent labelled according to `getOption("lgr.log_levels")`}
#'   \item{`values`}{`list`: All values stored in the LogEvent (including
#'     all *custom fields*, but not including `event$logger`)}
#'   \item{`logger_name`}{`character` scalar: The name of the Logger that
#'     created this event, equivalent to `event$logger$name`)}
#'   \item{`logger_user`}{`character` scalar: The user of the Logger that
#'     created this event, equivalent to `event$logger_user`)}
#' }
#'
#' @name LogEvent
#' @seealso [as.data.frame.LogEvent()]
#' @aliases LogEvents
#' @examples
#' l <- Logger$new("dummy logger", appenders = NULL)
#' l$error("foo bar")
#'
#' # The last LogEvent produced by a Logger is stored in the last_event field
#' l$last_event  # formatted by default
#' l$last_event$values  # values stored in the event
#'
#' # Also contains the Logger that created it as .logger
#' l$last_event$logger
#' # equivalent to
#' l$last_event$.logger$name
#'
#' # This is really a reference to the complete Logger, so the following is
#' # possible (though nonsensical)
#' l$last_event$.logger$last_event$msg
#' identical(l, l$last_event$.logger)
#'
NULL




#' @export
LogEvent <- R6::R6Class(
  "LogEvent",
  lock_objects = FALSE,
  public = list(
    initialize = function(
      logger,
      level = 400,
      timestamp = Sys.time(),
      caller = NA,
      msg = NA,
      ...
    ){
      assert(inherits(logger, "Logger"))

      # assign has less overhead than [[ and event creation needs to be as fast
      # as possible
      assign(".logger", logger, self)
      assign("level", level, self)
      assign("timestamp", timestamp, self)
      assign("caller", caller, self)
      assign("msg", msg, self)

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
    level = NULL,
    timestamp = NULL,
    caller = NULL,
    msg = NULL,
    .logger = NULL
  ),

  active = list(
    values = function(){
      fixed_vals   <- c("level", "timestamp", "logger", "caller", "msg")
      custom_vals <- setdiff(
        names(get(".__enclos_env__", self)[["self"]]),
        c(".__enclos_env__", "level_name", "initialize", "clone", "values",
          ".logger")
      )
      valnames <- union(fixed_vals, custom_vals) # to enforce order of fixed_vals
      mget(valnames, envir = self)
    },

    level_name = function(){
      label_levels(get("level", envir = self))
    },

    logger = function(){
      get("full_name", envir = get(".logger", envir = self))
    }
  )
)




#' Coerce LogEvents to Data Frames
#'
#' Coerce LogEvents to `data.frames`, [`data.tables`][data.table::data.table],
#' or [`tibbles`][tibble::tibble].
#'
#' @inheritParams base::as.data.frame
#' @param ... passed on to `data.frame()`
#' @param optional currently ignored
#' @param strict If `TRUE` as.data.frame will fail if `x` contains values that
#'   cannot be included in a data.frame. Ff `FALSE` (the default) they will
#'   be coerced
#' @export
#' @seealso [data.table::data.table], [tibble::tibble]
#'
#' @examples
#' l <- Logger$new("test")
#' l$info("lorem ipsum")
#' as.data.frame(l$last_event)
#'
#' l$info("rememver LogEvents can store any custom log values", df = iris)
#' as.data.frame(l$last_event)
#' head(as.data.frame(l$last_event)$df[[1]])
#'
as.data.frame.LogEvent <- function(
  x,
  row.names = NULL,
  optional = FALSE,
  stringsAsFactors = default.stringsAsFactors(),
  strict = FALSE,
  ...
){
  values <- x$values
  needs_boxing <- !vapply(values, is.atomic, logical(1))
  values[needs_boxing] <- lapply(values[needs_boxing], function(.x) I(list(.x)))

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
  ...
){
  values <- x$values
  needs_boxing <- !vapply(values, is.atomic, logical(1))
  values[needs_boxing] <- lapply(values[needs_boxing], function(.x) list(.x))
  data.table::as.data.table(values)
}




#' @rdname as.data.frame.LogEvent
as_tibble.LogEvent <- function(
  x,
  ...
){
  values <- x$values
  needs_boxing <- !vapply(values, is.atomic, logical(1))
  values[needs_boxing] <- lapply(values[needs_boxing], function(.x) list(.x))
  tibble::as_tibble(values)
}




# global variables --------------------------------------------------------

DEFAULT_FIELDS <- c("level", "timestamp", "logger", "caller", "msg")
