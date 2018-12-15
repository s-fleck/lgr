#' Filterables
#'
#' Superclass for classes that have a filter method, such as Appenders an
#' Loggers.
#'
#' @noRd
#'
Filterable <- R6::R6Class(
  "Filterable",
  cloneable = FALSE,

  public = list(
    filter = function(event){
      for (f in private$.filters) {
        if (!identical(f(event, self), TRUE)) return(FALSE)
      }
      TRUE
    }
  ),

  active = list(
    filters = function(value){
      if (missing(value))  return(private$.filters)
      assert(
        is.list(value) && all(vapply(value, is_filter, logical(1))),
        "'filters' must be a list of functions with the arguments 'event' and 'obj'"
      )
      private$.filters <- value
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
