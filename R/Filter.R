#'  Filter
#'
#'  @name Filter



#'  @export
Filter <- R6::R6Class(
  "Filter",
  public = list(
    filter = function(event, obj){TRUE}
  )
)



FilterInject <- R6::R6Class(
  "FilterInject",
  inherit = Filter,
  public = list(
    initialize = function(..., .list = list()){
      vals <- list(...)
      assert(!is.null(names(vals)) && is_equal_length(names(vals)), vals)
      if (length(list) > 0){
        assert(!is.null(names(.list)) && is_equal_length(names(.list)), .list)
      }
      vals <- c(vals, .list)
      assert(all_are_distinct(names(vals)))
      self[["values"]] <- vals
    },
    filter = function(event, obj){
      vals <- get("values", envir = self)
      for (i in seq_along(vals)){
        event[[names(vals)[[i]] ]] <- vals[[i]]
      }
      TRUE
    },
    values = NULL
  )
)



FilterForceLevel <- R6::R6Class(
  "FilterForceLevel",
  inherit = Filter,
  public = list(
    initialize = function(level){
      self[["level"]] <- standardize_log_level(level)
    },
    filter = function(event, obj){
      event[["level"]]  <- get("level", self)
      TRUE
    },
    level = NULL
  )
)



is_filter <- function(
  x
){
  inherits(x, "Filter") ||
  (is.function(x) && identical(names(formals(x)), c("event", "obj"))) ||
  (("filter" %in% names(x)) && is.function(x[["filter"]]) && is_filter(x[["filter"]]))
}
