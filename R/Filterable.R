#' Abstract Class for Filterables
#'
#' @template abstract_class
#'
#' @description
#' Superclass for classes that have a `filter()` method such as [Appenders] and
#' [Loggers]. This class is only exported for package developers that want to
#' extend it.
#' @name Filterable
#' @section Fields:
#'
#' \describe{
#'   \item{`filters`, `set_filters(filters)`}{a `list` of predicates (functions
#'     that return either `TRUE` or `FALSE`). These functions must have exactly
#'     two arguments: `event` and `obj`. When the `filter()` method of the
#'     Filterable is invoked on a LogEvent, that event will get passed to `event`,
#'     the Filterable will get passed to `obj`. This means that you can,
#'     f.e. use `obj$threshold` to access the threshold of the Appender/Logger
#'     that is using the filter.
#'     If all of these functions evaluate to `TRUE` the LogEvent is passed on.
#'     Since LogEvents have reference semantics, filters can also be abused to
#'     modify them before they are passed on. Look at the source code of
#'     [with_log_level()] or [with_log_value()] for examples.
#'   }
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{`filter(event)`}{Determine whether the LogEvent `x` should be passed
#'     on to Appenders (`TRUE`) or not (`FALSE`). See also the active binding
#'     `filters`}
#' }
#'
#' @keywords internal
NULL




#'  @rdname Filterable
#'  @export
Filterable <- R6::R6Class(
  "Filterable",
  cloneable = FALSE,

  public = list(
    filter = function(event){
      for (f in private$.filters) {
        if (!identical(f(event, self), TRUE)) return(FALSE)
      }
      TRUE
    },

    set_filters = function(filters){
      if (is.null(filters)){
        private$.filters <- NULL
      } else {
        assert(
          is.list(filters) && all(vapply(filters, is_filter, logical(1))),
          "'filters' must be a list of functions with the arguments 'event' and 'obj'"
        )
        private$.filters <- filters
      }

      invisible(self)
    }
  ),

  active = list(
    filters = function(){
      private$.filters
    }
  ),

  private = list(
    .filters = NULL
  )
)




is_filter <- function(
  x
){
  is.function(x) && identical(names(formals(x)), c("event", "obj"))
}
