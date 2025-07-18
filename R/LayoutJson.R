# LayoutJson --------------------------------------------------------------

#' Format LogEvents as JSON
#'
#' @description
#' A format for formatting LogEvents as
#' [jsonlines](https://jsonlines.org/) log files. This provides a
#' nice balance between human- an machine-readability.
#'
#' @family Layouts
#' @seealso [read_json_lines()], [https://jsonlines.org/](https://jsonlines.org/)
#' @export
#' @examples
#' # setup a dummy LogEvent
#' event <- LogEvent$new(
#'   logger = Logger$new("dummy logger"),
#'   level = 200,
#'   timestamp = Sys.time(),
#'   caller = NA_character_,
#'   msg = "a test message",
#'   custom_field = "LayoutJson can handle arbitrary fields"
#' )
#'
#' lo <- LayoutJson$new()
#' lo$format_event(event)
#'
#' lo <- LayoutJson$new(
#'   transform_event_names = toupper,
#'   excluded_fields = c("RAWMSG", "CALLER"))
#'
#' lo$format_event(event)
#'
#' lo <- LayoutJson$new(
#'   transform_event = function(e) {
#'     values <- e$values
#'     values$msg <- toupper(values$msg)
#'     values
#'   },
#'   timestamp_fmt = "%a %b %d %H:%M:%S %Y",
#'   excluded_fields = c("RAWMSG", "CALLER"))
#'
#' lo$format_event(event)
LayoutJson <- R6::R6Class(
  "LayoutJson",
  inherit = Layout,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param toJSON_args a list of arguments passed to [jsonlite::toJSON()],
    #'
    #' @param transform_event a `function` with a single argument that
    #'   takes a [LogEvent] object and returns a `list` of values.
    #'
    #' @param timestamp_fmt Format to be applied to the timestamp. This is
    #'   applied after `transform_event` but `before transform_event_names`
    #'   * `NULL`: formatting of the timestamp is left to [jsonlite::toJSON()],
    #'   * a `character` scalar as for [format.POSIXct()], or
    #'   * a `function` that returns a vector of the same length as its
    #'     ([POSIXct]) input. The returned vector can be of any type
    #'     supported by [jsonlite::toJSON()].
    #'
    #' @param transform_event_names
    #' * `NULL`: don't process names
    #' * a named `character` vector where the names are the original field names
    #'   and the values the desired new field names,
    #' * or a `function` with a single mandatory argument that accepts a
    #'   `character` vector of field names. Applied after `transform_event`.
    #'
    #' @param excluded_fields A `character` vector of field names to exclude
    #'   from the final output. Applied after `transform_event_names`.
    initialize = function(
      toJSON_args = list(auto_unbox = TRUE),
      timestamp_fmt = NULL,
      transform_event = function(event) event[["values"]],
      transform_event_names = NULL,
      excluded_fields = "rawMsg"
    ){
      self$set_toJSON_args(toJSON_args)
      self$set_timestamp_fmt(timestamp_fmt)
      self$set_transform_event(transform_event)
      self$set_transform_event_names(transform_event_names)
      self$set_excluded_fields(excluded_fields)
    },

    format_event = function(event) {
      values <- get(".transform_event", private)(event)
      values[["timestamp"]] <- apply_timestamp_formatter(values[["timestamp"]], get(".timestamp_fmt", private))
      names(values) <- apply_event_name_transformer(names(values), get(".transform_event_names", private))
      values <- apply_field_exclusion(values, self$excluded_fields)

      do.call(
        jsonlite::toJSON,
        args = c(list(x = values), get(".toJSON_args", private))
      )
    },

    # . . setters -------------------------------------------------------------

    #' @param x a `list`
    set_toJSON_args = function(x){
      assert(is.list(x))
      assert(identical(length(names(x)), length(x)))
      private$.toJSON_args <- x
      invisible(self)
    },

    #' @param x a `character` scalar or a `function` that accepts a `POSIXct`
    #'   as its single argument
    set_timestamp_fmt = function(x){
      assert(is.null(x) || is_scalar_character(x) || is.function(x))
      private[[".timestamp_fmt"]] <- x
      invisible(self)
    },

    #' @param x a `function` that accepts a `LogEvent` as its single argument
    set_transform_event = function(x){
      assert(
        is.function(x) && length(formals(x)) >= 1L,
        "`transform_event` must be a function a single argument (optional arguments are OK)")

      private[[".transform_event"]] <- x
      invisible(self)
    },

    #' @param x a named `character` vector or a function that accepts a
    #'   `character` vector of field names as its single argument.
    set_transform_event_names = function(x){
      assert(
        is.null(x) || is_field_name_map(x) || (is.function(x) && length(formals(x)) >= 1L),
        "`transform_event_names` must be a named character vector or function with a single mandatory argument (optional arguments are OK)")

      private[[".transform_event_names"]] <- x
    },

    #  . . methods ----------------------------------------------------------------

    #' @description Represent the `LayoutJson` class as a string
    toString = function() {
      fmt_class(class(self)[[1]])
    },

    #' @description Read and parse file written using this Layout
    #'
    #' This can be used by the `$data` active binding of an [Appender]
    #'
    #' @param file `character` scalar: path to a file
    parse = function(file){
      read_json_lines(file)
    },

    #' @description Read a file written using this Layout (without parsing)
    #'
    #' This can be used by the `$show()` method of an [Appender]
    #'
    #' @param file `character` scalar: path to a file
    #' @param threshold `character` Minimum log level to show. Requires parsing
    #'  of the log file (but will still display unparsed output)
    #' @param n `integer` number of lines to show
    read = function(
      file,
      threshold = NA_integer_,
      n = 20L
    ){
      assert(is_scalar_integerish(n))
      threshold <- standardize_threshold(threshold)

      dd <- readLines(file)
      if (!is.na(threshold)){
        sel <- self$parse(file)$level <= threshold
      } else {
        sel <- TRUE
      }

      dd <- tail(dd[sel], n)
      dd
    }
  ),


  #  . . active fields ------------------------------------------------------

  active = list(

    #' @field toJSON_args a `list`
    toJSON_args = function() {
      get(".toJSON_args", private)
    },

    #' @field timestamp_fmt a `character` scalar or a `function` that accepts a `POSIXct`
    #'   as its single argument
    timestamp_fmt = function() {
      get(".timestamp_fmt", private)
    },

    #' @field transform_event a `function` that accepts a `LogEvent` as its single argument
    transform_event = function(){
      get(".transform_event", private)
    },

    #' @field transform_event_names a named `character` vector or a function that accepts a
    #'   `character` vector of field names as its single argument.
    transform_event_names = function() {
      get(".transform_event_names", private)
    }
  ),

  # . . private --------------------------------------------------------------
  private = list(
    .toJSON_args = NULL,
    .timestamp_fmt = NULL,
    .transform_event = NULL,
    .transform_event_names = NULL
  )
)


# utils -------------------------------------------------------------------

apply_timestamp_formatter = function(x, f){
  if (is.null(f)){
    return(x)
  }

  if (is.character(f)){
    return(format(x, f))
  }

  if (is.function(f)){
    return(f(x))
  }

  warning("`f` must be a character scalar or a function")
}


apply_event_name_transformer = function(x, f){
  if (is.null(f)){
    return(x)
  }

  if (is.character(f)){
    rename_idx <- match(x, names(f), nomatch = 0L)
    x[rename_idx > 0L] <- f[rename_idx[rename_idx > 0L]]
    return(x)
  }

  if(is.function(f)){
    return(f(x))
  }

  warning("`f` must be a named character vector or a function")
  x
}


apply_field_exclusion <- function(x, f){
  if (is.null(f)){
    return(x)
  }

  x[!names(x) %in% f]
}


is_field_name_map <- function(x){
  is.character(x) && !is.null(names(x)) && all(nzchar(names(x)))
}
