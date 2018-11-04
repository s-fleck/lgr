
#' @param filters a list of functions with the arguments x and self
Filterable <- R6::R6Class(
  "Filterable",

  public = list(
    filter = function(x){
      for (f in private$.filters) {
        if (!identical(f(x, self), TRUE)) return(FALSE)
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
  x,
  self
){
  is.na(self$threshold) || x[["level"]] <= self$threshold
}
