#' Abstract Class for Filterables
#'
#' @description
#' Superclass for classes that have a `$filter()` method such as [Appenders] and
#' [Loggers]. See [EventFilter] for Details.
#'
#' @template abstract_class
#'
#' @export
Filterable <- R6::R6Class(
  "Filterable",
  cloneable = FALSE,

  public = list(

    #' @description Determine whether the LogEvent `x` should be passed on to
    #' Appenders (`TRUE`) or not (`FALSE`). See also the active binding
    #' `filters`
    #'
    #' @param event a [LogEvent]
    filter = function(event){
      for (f in get(".filters", private)) {

        if (is.function(f)){
          r <- f(event)
        } else if (is_filter(f)){
          r <- f[["filter"]](event)
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

    #' @description Add a Filter. When adding a filter an optional `name`
    #'   can be specified. `remove_filter()` can remove by position or name (if
    #'   one was specified)
    #' @param filter a function that returns `TRUE` or `FALSE`, a
    #'   [Filter][EventFilter] or any \R object with a `$filter()` method.
    #'
    #' @param name `character` scalar or `NULL`. A filter can have an optional
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


    #' @description Replace all filters with a list of either `functions` or
    #'   arbitrary \R object with a `$filter()` method (preferably a [Filter] R6
    #'   object). These functions/methods must have exactly one argument:
    #'   `event` which will get passed the LogEvent when the Filterable's
    #'   `$filter()` method is invoked. If all of these functions evaluate to
    #'   `TRUE` the LogEvent is passed on. Since LogEvents have reference
    #'   semantics, filters can also be abused to modify them before they are
    #'   passed on. Look at the source code of [with_log_level()] or
    #'   [with_log_value()] for examples.
    #'
    #' @param filters a `list` (named or unnamed) of [Filter][Filters] or
    #' predicate functions.
    set_filters = function(filters){
      filters <- standardize_filters_list(filters)
      private[[".filters"]] <- filters
      invisible(self)
    }
  ),

  active = list(
    #' @field filters a `list` of all attached filters (either [Filter] R6
    #' objects or `functions`).
    filters = function(){
      get(".filters", private)
    }
  ),

  private = list(
    .filters = list()
  )
)
