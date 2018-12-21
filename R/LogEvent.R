#' Events - The Atomic Unit of Logging
#'
#' A `LogEvent` is a single unit of data that should be logged. It usually
#' contains at minimum the [log_level], a timestamp, the name of the calling
#' function, the log message and a reference to the Logger that created it.
#' `LogEvents` are created by a [Logger], and then processed by an [Appenders].
#'
#' @eval r6_usage(LogEvent)
#'
#'
#' @section Creating LogEvents:
#'
#' **not yet stable**
#' Currently custom LogEvents are not officially supported in yog, but a lot of
#' the infrastructure to make them possible is already in place. If you require
#' this function, please upvote
#' [this issue](https://github.com/s-fleck/yog/issues/4)
#'
#' \describe{
#'   \item{logger}{[`Logger`] scalar. The `Logger` this `LogEvent` is
#'     associated with}
#'  }
#'
#' @section Fields:
#'
#' Log Evnets contain the following data fields
#'
#' * `level`: The [log_level] of the event
#' * `timestamp`: `POSIXct`.
#' * `caller`: `character`. the function that called this event
#' * `msg`: `character`. the log message.
#' * `logger` the `Logger` that created the `LogEvent`. See examples.
#'
#' Usually the above values will be scalars, but (except for `"logger"`) they
#' can also be vectors if they are all of the same length (or scalars that will
#' be recycled). In this case the event will be treated by the [Appenders] and
#' [Layouts] as if several separate events.
#'
#' Log events also contain the following active bindings:
#'
#' * `value`: an active binding that returns a named `list` containing all the
#'   above values.
#' * `level_name`  the log level as a `character` string.
#'
#'
#' @name LogEvent
#' @aliases LogEvents
#' @examples
#'
#' # Usually you do not create a LogEvent in this manner, it is rather provided
#' # by the Logger
#'
#' l <- Logger$new("dummy logger", appenders = NULL)
#' l$error("foo bar")
#'
#' # The last LogEvent produced by a logger is saved to the last_event field
#' l$last_event
#' l$last_event$level
#' l$last_event$level_name
#' l$last_event$msg
#'
#' # Also contains a reference to the complete logger that created it
#' l$last_event$logger$name
#' l$last_event$logger$user
#'
#' # This is really a reference to the complete logger, so the following is
#' # also possible (though nonsensical)
#' l$last_event$logger$last_event$msg
#' identical(l, l$last_event$logger)
#'
#' # Because LogEvents can also be vectors, the following works:
#' l$warn(c("a", "vector", "log", "message"))
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
      assign("logger", logger, self)
      assign("level", level, self)
      assign("timestamp", timestamp, self)
      assign("caller", caller, self)
      assign("msg", msg, self)

      # custom values
      if (!missing(...)){
        dots <- list(...)
        assert(identical(length(names(dots)), length(dots)))
        for (nm in names(dots)){
          assign(nm, dots[[nm]], self)
        }
      }

      invisible(self)
    },
    logger = NULL,
    level = NULL,
    timestamp = NULL,
    caller = NULL,
    msg = NULL
  ),

  active = list(
    values = function(){
      fixed_vals   <- c("level", "timestamp", "caller", "msg")
      custom_vals <- setdiff(
        names(self[[".__enclos_env__"]][["self"]]),
        c(".__enclos_env__", "level_name", "initialize", "clone", "values", "logger")
      )
      valnames <- union(fixed_vals, custom_vals)
      mget(valnames, envir = self)
    },
    level_name = function(){
      label_levels(self$level)
    }
  )
)




#' Coerce LogEvents to Data Frames
#'
#' @inheritParams base::as.data.frame
#' @param ... passed on to `as.data.frame.list`
#' @export
as.data.frame.LogEvent <- function(
  x,
  row.names = NULL,
  optional = FALSE,
  stringsAsFactors = default.stringsAsFactors(),
  ...
){
  as.data.frame(
    x$values,
    row.names = row.names,
    optional = FALSE,
    stringsAsFactors = stringsAsFactors,
    ...
  )
}
