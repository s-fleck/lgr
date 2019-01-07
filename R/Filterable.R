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
#'   \item{`filters`, `set_filters(filters)`}{a `list` that may contain
#'     functions or any \R object with a `filter()` method. These functions must
#'     have exactly two arguments: `event` and `obj`. When the `filter()` method
#'     of the Filterable is invoked on a LogEvent, that event will get passed to
#'     `event`, the Filterable will get passed to `obj`. This means that you
#'     can, f.e. use `obj$threshold` to access the threshold of the
#'     Appender/Logger that is using the filter. If all of these functions
#'     evaluate to `TRUE` the LogEvent is passed on. Since LogEvents have
#'     reference semantics, filters can also be abused to modify them before
#'     they are passed on. Look at the source code of [with_log_level()] or
#'     [with_log_value()] for examples.
#'   }
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{`filter(event)`}{Determine whether the LogEvent `x` should be passed
#'     on to Appenders (`TRUE`) or not (`FALSE`). See also the active binding
#'     `filters`}
#'    \item{`add_filter(filter, name = NULL)`, `remove_filter(pos)`}{
#'      Add or remove a filter. When adding a filter an optional `name` can
#'      be specified. `remove_filter()` can remove by position or name (if one
#'      was specified)
#'    }
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
      for (f in get(".filters", private)) {

        if (is.function(f)){
          r <- f(event, self)
        } else if (inherits(f, "Filter")){
          r <- f[["filter"]](event, self)
        }

        if (identical(r, TRUE)){
          # do nothing
        } else if (identical(r, FALSE)){
          return(FALSE)
        } else {
          warning(
            "`$filter()` of ", class_fmt(self, c("R6", "Filterable")),
            " object did not return `TRUE` or `FALSE` but ", preview_object(r),
            ". Please check its `$filters`", call. = FALSE
          )
        }
      }
      TRUE
    },

    add_filter = function(filter, name = NULL){
      assert_filter(filter)
      assert(is.null(name) || is_scalar_character(name))
      pos <- name %||% (length(private$.filters) + 1L)
      private[[".filters"]][[pos]] <- filter
      invisible(self)
    },

    remove_filter = function(pos){
      private[[".filters"]][[pos]] <- NULL
      invisible(self)
    },

    set_filters = function(filters){
      if (is.null(filters)){
        private[[".filters"]] <- NULL
      } else {
        assert(
          is.list(filters) && all(vapply(filters, is_filter, logical(1))),
          "'filters' must be a list of functions with the arguments 'event' and 'obj'"
        )
        private[[".filters"]] <- filters
      }

      invisible(self)
    }
  ),

  active = list(
    filters = function(){
      get(".filters", private)
    }
  ),

  private = list(
    .filters = list()
  )
)




assert_filter <- function(x){
  if (is_filter(x))
    TRUE
  else
    stop(
      "`", deparse(substitute(x)), "`", "
      must be a function with the arguments `event` and `obj`",
      call. = FALSE
    )
}
