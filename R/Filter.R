#' Event Filters
#'
#' @description
#' Filters can be used for the `$set_filter()` and `$add_filter()` methods of
#' Appenders and Loggers. You normally do not need to construct a formal
#' `EventFilter` object, you can just use any `function` that has the single
#' argument `event`, or any object that has a `filter` method.
#'
#' NOTE: You can use the special function [.obj()] to access the calling
#' Logger/Appender from within a filter
#'
#' @section Modifying LogEvents with Filters:
#'
#' Since LogEvents are R6 objects with reference semantics, Filters can
#' be abused to modify log events before passing them on. lgr comes with a few
#' preset filters that use this property: [FilterInject]
#' (similar to [with_log_level()]) and [FilterForceLevel] (similar to [with_log_value()]).
#'
#' The base class for Filters is called `EventFilter` so that it doesn't
#' conflict with [base::Filter()]. The recommended convention for Filter
#' subclasses is to call them `FilterSomething` and leave out the
#' `Event` prefix.
#'
#' @aliases Filter
#' @export
EventFilter <- R6::R6Class(
  "EventFilter",
  public = list(

    #' @description Initilize a new EventFilter
    #' @param fun a `function` with a single argument `event` that must return
    #'   either `TRUE` or `FALSE.`
    initialize = function(fun = function(event) TRUE){
      self$filter <- fun
    },

    #' @description filter
    #' The `filter` method of an event must return either `TRUE` or `FALSE`. If
    #' it returns `TRUE`, [Filterables] such as [Appenders] or [Loggers] will
    #' continue processing the event, if `FALSE` the event will be discarded.
    filter = NULL
  )
)




#' Inject values into all events processed by a Logger/Appender
#'
#' @description
#' Inject arbitrary values into all [LogEvents][LogEvent] processed by a
#' Logger/Appender.  It is recommended to use this filter only with Loggers,
#' but it will also work on Appenders.
#'
#' @export
#' @examples
#' lg <- get_logger("test")
#'
#' analyse <- function(){
#'   lg$add_filter(FilterInject$new(type = "analysis"), "inject")
#'   on.exit(lg$remove_filter("inject"))
#'   lg$error("an error with forced custom 'type'-field")
#' }
#'
#' analyse()
#' lg$error("an normal error")
#' lg$config(NULL)  # reset config
FilterInject <- R6::R6Class(
  "FilterInject",
  inherit = EventFilter,
  public = list(

    #' @description Initilize a new FilterInject
    #' @param ...,.list any number of named \R objects that will be injected as
    #'   custom fields into all [LogEvents][LogEvent] processed by the
    #'   Appender/Logger that this filter is attached to.
    #'   See also [with_log_value()].
    initialize = function(..., .list = list()){
      vals <- list(...)
      assert(!is.null(names(vals)) && is_equal_length(names(vals)), vals)
      if (length(.list) > 0){
        assert(!is.null(names(.list)) && is_equal_length(names(.list)), .list)
      }
      vals <- c(vals, .list)
      assert(all_are_distinct(names(vals)))
      self[["values"]] <- vals

      self$filter <- function(event){
        vals <- get("values", envir = self)
        for (i in seq_along(vals)){
          event[[names(vals)[[i]] ]] <- vals[[i]]
        }
        TRUE
      }
    },

    #' @field values a named `list` of values to be injected into each [LogEvent]
    #'   processed by this filter
    values = NULL
  )
)




#' Override the log level of all events processed by a Logger/Appender
#'
#'
#' @description
#' Overrides the log level of the  Appender/Logger that this filter is attached
#' to to with `level`. See also [with_log_level()]. It is recommended to use
#' this filter only with Loggers, but it will also work on Appenders.
#'
#' @export
#' @examples
#' lg <- get_logger("test")
#'
#' analyse <- function(){
#'   lg$add_filter(FilterForceLevel$new("info"), "force")
#'   on.exit(lg$remove_filter("force"))
#'   lg$error("an error with forced log level INFO")
#' }
#'
#' analyse()
#' lg$error("an normal error")
#' lg$config(NULL)  # reset config
FilterForceLevel <- R6::R6Class(
  "FilterForceLevel",
  inherit = EventFilter,
  public = list(

    #' @description Initilize a new FilterForceLevel
    #' @param level an `integer` or `character` [log level][log_level]
    initialize = function(level){
      self[["level"]] <- standardize_log_level(level)

      self[["filter"]] <- function(event){
        event[["level"]]  <- get("level", self)
        TRUE
      }
    },

    #' @field level an `integer` [log level][log_level] used to override the log
    #' levels of each [LogEvent] processed by this filter.
    level = NULL
  )
)



#' Check if an R Object is a Filter
#'
#' @param x any \R Object
#' @seealso [EventFilter]
#'
#' @export
is_filter <- function(
  x
){
  if (is.function(x)){
    identical(names(formals(x)), c("event"))
  } else {
    # the extra is.function is to prevent infinite recursions
    "filter" %in% names(x) &&
    is.function(x[["filter"]]) &&
    identical(names(formals(x[["filter"]])), c("event"))
  }
}




#' Retrieve the Parent object of a Filter
#'
#' Only for use within `$filter()` methods of [EventFilters][EventFilter]
#'
#' @export
#' @examples
#' lg <- get_logger("test")
#' f <- function(event) {
#'   cat("via event$.logger:", event$.logger$threshold, "\n")  #  works for loggers only
#'   cat("via .obj():      ",.obj()$threshold, "\n") # works for loggers and appenders
#'   TRUE
#' }
#' lg$add_filter(f)
#' lg$fatal("test")
#' lg$config(NULL)
.obj <- function(){
  get("self", parent.env(parent.frame(2)))
}

