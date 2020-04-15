# EventFilter -------------------------------------------------------------

#' Event Filters
#'
#' @description EventFilters specify arbitrarily complex logic for whether or
#'   not a LogEvent should be processed by a [Logger] or [Appender]. They are
#'   attached to Loggers/Appenders via their `$set_filter()` or `$add_filter()`
#'   methods. If any EventFilter evaluates to `FALSE` for a given event, that
#'   event is ignored - similarly to when it does not pass the objects'
#'   threshold.
#'
#'   Usually you do not need to instantiate a formal `EventFilter` object as you
#'   can just use any `function` that has the single argument `event` instead.
#'   If you need to implement more complex filter logic - for example a filter
#'   that is dependent on a dataset - it might be desirable to subclass
#'   EventFilter, as [R6::R6] objects can store data and functions together.
#'
#'
#' @section Modifying LogEvents with EventFilters:
#'
#'   Since LogEvents are R6 objects with reference semantics, EventFilters can be
#'   abused to modify events before passing them on. lgr comes with a few
#'   preset filters that use this property: [FilterInject] (similar to
#'   [with_log_level()]) and [FilterForceLevel] (similar to [with_log_value()]).
#'
#'   **NOTE:** The base class for Filters is called `EventFilter` so that it
#'   doesn't conflict with [base::Filter()]. The recommended convention for
#'   Filter subclasses is to call them `FilterSomething` and leave out the
#'   `Event` prefix.
#'
#' @aliases Filter
#' @seealso [is_filter()]
#' @export
EventFilter <- R6::R6Class(
  "EventFilter",
  public = list(

    #' @description Initialize a new EventFilter
    #' @param fun a `function` with a single argument `event` that must return
    #'   either `TRUE` or `FALSE`. Any  non-`FALSE` will be interpreted as
    #'   `TRUE` (= no filtering takes place) and a warning will be thrown.
    initialize = function(fun = function(event) TRUE){
      self$filter <- fun
    },

    #' @description filter The `filter` method of an event must return either
    #'   `TRUE` or `FALSE`. If it returns `TRUE`, [Filterables] such as
    #'   [Appenders] or [Loggers] will continue processing the event, if `FALSE`
    #'   the event will be discarded.
    filter = NULL
  )
)




# FilterInject ------------------------------------------------------------

#' Inject values into all events processed by a Logger/Appender
#'
#' @description Inject arbitrary values into all [LogEvents][LogEvent] processed
#' by a Logger/Appender. It is recommended to use filters that modify LogEvents
#' only with Loggers, but they will also work with Appenders.
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

    #' @description Initialize a new FilterInject
    #' @param ...,.list any number of named \R objects that will be injected as
    #'   custom fields into all [LogEvents][LogEvent] processed by the
    #'   Appender/Logger that this filter is attached to. See also
    #'   [with_log_value()].
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

    #' @field values a named `list` of values to be injected into each
    #'   [LogEvent] processed by this filter
    values = NULL
  )
)




# FilterForceLevel --------------------------------------------------------

#' Override the log level of all events processed by a Logger/Appender
#'
#' @description Overrides the log level of the  Appender/Logger that this filter
#'   is attached to to with `level`. See also [with_log_level()]. It is
#'   recommended to use filters that modify LogEvents only with Loggers, but
#'   they will also work with Appenders.
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

    #' @description Initialize a new FilterForceLevel
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




# utils -------------------------------------------------------------------

#' @description `.obj()` is a special function that can only be used within the
#' `$filter()` methods of [EventFilters][EventFilter]. It returns the [Logger]
#' or [Appender] that the EventFilter is attached to.
#'
#' @rdname EventFilter
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




#' Check if an R Object is a Filter
#'
#' @description Returns `TRUE` for any \R object that can be used as a Filter
#' for [Loggers] or, [Appenders]:
#' * a `function` with the single argument `event`;
#' * an [EventFilter] [R6::R6] object; or
#' * any object with a `$filter(event)` method.
#'
#' **Note:** A Filter **must** return a scalar `TRUE` or `FALSE`, but this
#' property cannot be checked by [is_filter()].
#'
#' @param x any \R Object
#' @seealso [EventFilter], [Filterable]
#' @return `TRUE` or `FALSE`
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




standardize_filters_list <- function(x){
  if (is.null(x))
    return(list())

  if (is_filter(x))
    return(list(x))

  assert(
    is.list(x),
    "'filters' must be a list Filters or a single Filter (see ?is_filter)"
  )

  for (f in x)
    assert_filter(f)

  x
}




assert_filter <- function(x){
  if (is_filter(x))
    TRUE
  else
    stop(ObjectIsNoFilterError(paste0(
      "`", deparse(substitute(x)), "` ",
      "is not a function with the single argument `event` or an EventFilter, ",
      "but ", preview_object(x), ". See ?is_filter."
    )
  ))
}




ObjectIsNoFilterError <- function(message = "Object is not an EventFilter. See ?is_filter."){
  error(message, "ObjectIsNoFilterError")
}
