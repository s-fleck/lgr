#' LogEvents - The Atomic Unit of Logging
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
    .logger = NULL
  ),

  active = list(

    #' @field values `list`. All values stored in the `LogEvent`, including
    #' all *custom fields*, but not including `event$.logger`.
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
#' @param needs_boxing a `function` that returns `TRUE` or `FALSE` to determine
#'   wich values are to be boxed (i.e. placed as single elements in a list
#'   column). See example
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
#' # by default only recursive structures (such as lists) are boxed
#' lg$info("letters", letters = as.list(letters))
#' as.data.frame(lg$last_event)
#' # but vectors are not
#' lg$info("letters", letters = letters)
#' as.data.frame(lg$last_event)
#' # this behaviour can be modified by supplying a custom boxing function
#' as.data.frame(lg$last_event, needs_boxing = function(.) length(.) > 1)
#'
as.data.frame.LogEvent <- function(
  x,
  row.names = NULL,
  optional = FALSE,
  stringsAsFactors = FALSE,
  ...,
  box_if = function(.) !(is.atomic(.) && identical(length(.), 1L)),
  cols_expand  = "msg"
){
  boxer <- function(.) I(list(.))
  values <- do_box_if(x$values, box_if, except = cols_expand, boxer = boxer)
  msg_len <- length(x[["msg"]])
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



# global variables --------------------------------------------------------

DEFAULT_FIELDS <- c("level", "timestamp", "logger", "caller", "msg")
