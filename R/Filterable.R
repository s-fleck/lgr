
#' @param filters a list of functions with the arguments x and self
Filterable <- R6::R6Class(
  "Filterable",
  cloneable = FALSE,

  public = list(
    filter = function(record){
      for (f in private$.filters) {
        if (!identical(f(record, self), TRUE)) return(FALSE)
      }
      TRUE
    }
  ),

  active = list(
    filters = function(value){
      if (missing(value))  return(private$.filters)
      assert(
        is.list(value) && all(vapply(value, is_filter, logical(1))),
        "'filters' must be a list of functions with the arguments 'x' and 'self'"
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
  is.function(x) && identical(names(formals(x)), c("x", "self"))
}




check_threshold <- function(
  record,
  self
){
  is.na(self$threshold) || record[["level"]] <= self$threshold
}
