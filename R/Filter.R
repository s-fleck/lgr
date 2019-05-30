#' Event Filters
#'
#' Filters can be used for the `$set_filter()` and `$add_filter()` methods of
#' Appenders and Loggers. You normally do not need to construct a formal
#' `EventFilter` object, you can just use any `function` that has the single
#' argument `event` or any object that has a `filter` method.
#'
#' @section Modifying LogEvents with Filters:
#'
#' Since LogEvents are R6 objects with reference semantics, Filters can also
#' be abused to modify log events before passing them on. lgr comes with a few
#' preset filters that use this property:
#'
#' \describe{
#'
#' \item{`FilterInject$new(..., .list)`}{`...` and `.list` can take any number
#'   of named R6 objects that will be injected as custom fields into all
#'   LogEvents processed by the  Appender/Logger that this filter is attached
#'   to. See also [with_log_value()]}
#'
#' \item{`FilterForceLevel$new(level)`}{Sets the level of all LogEvents
#'   processed by the Appender/Logger that this filter is attached to to
#'   `level`. See also [with_log_value()]}
#' }
#'
#' @note
#' The base class for Filters is called `EventFilter` so that it doesn't
#' conflict with [base::Filter()]. The recommended convention for Filter
#' subclasses is to call them `FilterSomething` and leave out the
#' `Event` prefix.
#'
#'
#' @section Accessing Appenders and Loggers from Filters:
#'
#' You can use the special function `.obj()` to access the calling
#' Logger/Appender from within a filter
#'
#'
#' @aliases Filter FilterInject FilterForceLevel
#' @name EventFilter
#'
#' @examples
#' # using filters to modify log events
#' lg <- get_logger("test")
#'
#' analyse <- function(){
#'   lg$add_filter(FilterForceLevel$new("info"), "force")
#'   lg$add_filter(FilterInject$new(type = "analysis"), "inject")
#'   on.exit(lg$remove_filter(c("force", "inject")))
#'   lg$debug("a debug message")
#'   lg$error("an error")
#' }
#' analyse()
#' lg$error("an error")
#' lg$config(NULL)  # reset config
#'
#'
#' # using .obj()
#' lg <- get_logger("test")
#' f <- function(event) {
#'   cat("via event$.logger:", event$.logger$threshold, "\n")  #  works for loggers only
#'   cat("via .obj():      ",.obj()$threshold, "\n") # works for loggers and appenders
#'   TRUE
#' }
#' lg$add_filter(f)
#' lg$fatal("test")
#' lg$config(NULL)
NULL


#' @export
EventFilter <- R6::R6Class(
  "EventFilter",
  public = list(
    initialize = function(fun = function(event){TRUE}){
      self$filter <- fun
    },
    filter = NULL
  )
)

#' @export
FilterInject <- R6::R6Class(
  "FilterInject",
  inherit = EventFilter,
  public = list(
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
    values = NULL
  )
)


#' @export
FilterForceLevel <- R6::R6Class(
  "FilterForceLevel",
  inherit = EventFilter,
  public = list(
    initialize = function(level){
      self[["level"]] <- standardize_log_level(level)

      self[["filter"]] <- function(event){
        event[["level"]]  <- get("level", self)
        TRUE
      }
    },
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




#' @export
#' @rdname Filterable
.obj <- function(){
  get("self", parent.env(parent.frame(2)))
}

