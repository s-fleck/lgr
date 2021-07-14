#' Abstract Class for Filterables
#'
#' @description Superclass for classes that have a `$filter()` method such as
#' [Appenders] and [Loggers]. See [EventFilter] for details.
#'
#' @template abstract_class
#'
#' @export
Filterable <- R6::R6Class(
  "Filterable",
  cloneable = FALSE,

  public = list(

    #' @description Determine whether the LogEvent `x` should be passed on to
    #'   Appenders (`TRUE`) or not (`FALSE`). See also the active binding
    #'   `filters`.
    #'
    #' @param event a [LogEvent]
    filter = function(event){
      for (f in get(".filters", private)) {

        # we can assume f() is a valid Filter since it is aleady verrified by
        # $set_filters()
        if (is.function(f)){
          r <- f(event)
        } else {
          r <- f[["filter"]](event)
        }

        if (identical(r, TRUE)){
          # do nothing
        } else if (identical(r, FALSE)){
          return(FALSE)
        } else {
          warning(
            "`$filter()` of ", class_fmt(self, c("R6", "Filterable")),
            " object did not return `TRUE` or `FALSE` but ", string_repr(r),
            ". Please check its `$filters`", call. = FALSE
          )
        }
      }
      TRUE
    },

    #' @description Attach a filter
    #' @param filter
    #' * a function with the single argument `event` that returns `TRUE`
    #'   or `FALSE`;
    #' * an [EventFilter] [R6::R6] object; or
    #' * any \R object with a `$filter()` method.
    #'
    #'  If a Filter returns a non-`FALSE` value, will be interpreted as `TRUE`
    #'  (= no filtering takes place) and a warning will be thrown.
    #'
    #' @param name `character` scalar or `NULL`. An optional
    #' name which makes it easier to access (or remove) the filter
    add_filter = function(filter, name = NULL){
      assert_filter(filter)
      assert(is.null(name) || is_scalar_character(name))
      pos <- name %||% (length(private$.filters) + 1L)
      private[[".filters"]][[pos]] <- filter
      invisible(self)
    },


    #' @description Remove a filter
    #' @param pos `character` or `integer` scalar. The name or index of the
    #' Filter to be removed.
    remove_filter = function(pos){
      if (is.numeric(pos)) sort(pos, decreasing = TRUE)
      for (p in pos){
        private[[".filters"]][[p]] <- NULL
      }
      invisible(self)
    },


    #' @description Set or replace (all) Filters of parent object. See
    #' [EventFilter] for how Filters work.
    #'
    #' @param filters a `list` (named or unnamed) of [EventFilters][EventFilter]
    #'   or predicate functions. See [is_filter()].
    set_filters = function(filters){
      filters <- standardize_filters_list(filters)
      private[[".filters"]] <- filters
      invisible(self)
    }
  ),

  active = list(
    #' @field filters a `list` of all attached Filters.
    filters = function(){
      get(".filters", private)
    }
  ),

  private = list(
    .filters = list()
  )
)
