#' Abstract Class for Filterables
#'
#' Superclass for classes that have a `filter()` method such as Appenders and
#' Loggers. This class is only exported for package developers that want to
#' extend it.
#' @name Filterable
#' @section Fields and Methods:
#'
#' \describe{
#'   \item{`filter(event)`}{Check if an event passes all filters}
#'   \item{`filters`,`set_filters(filters)`}{get/set a list of `filters`. A
#'   filter is a function that takes exactly two named arguments: the LogEvent
#'   `event` and the Filterable `obj` (usually a Logger or Appender)}
#'  }
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
      assert(
        is.list(filters) && all(vapply(filters, is_filter, logical(1))),
        "'filters' must be a list of functions with the arguments 'event' and 'obj'"
      )
      private$.filters <- filters
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




check_threshold <- function(
  event,
  obj
){
  is.na(obj$threshold) || event[["level"]] <= obj$threshold
}
