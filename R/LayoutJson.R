# LayoutJson --------------------------------------------------------------

#' Format LogEvents as JSON
#'
#' @description
#' A format for formatting LogEvents as
#' [jsonlines](https://jsonlines.org/) log files. This provides a
#' nice balance between human- an machine-readability.
#'
#'
#' @section Event transformation:
#' This Layout provides 4 ways to transform the event before serialization:
#'
#' 1. `transform_event`: a generic function to transform the event
#' 2. `timestamp_fmt`: a format string or function to apply to the timestamp field
#' 3. `transform_event_names`: a named `character` vector or a second
#'     function to rename fields
#' 4. `excluded_fields`: a `character` vector to include fields.
#'
#' In theory supplying a custom `transform_event` function is enough to
#' perform all these actions, but the other three parameters are provided for
#' convenience. Please note that they are applied in order (e.g. if you
#' rename a field you have to exclude the *renamed* field to really exclude it).
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
#' # Default settings show all event fals
#' lo <- LayoutJson$new()
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
    #' @param timestamp_fmt Format to be applied to the timestamp. This is
    #'   applied after `transform_event()` but `before transform_event_names()`
    #' * `NULL` (the default): formatting of the timestamp is left to
    #' [jsonlite::toJSON()],
    #' * a `character` scalar as for [format.POSIXct()], or
    #' * a `function` that returns a vector of the same length as its
    #'   ([POSIXct]) input. The returned vector can be of any type
    #'   supported by [jsonlite::toJSON()], but should usually be `character`.
    #'
    #' @param transform_event a `function` with a single argument `event` that
    #'   takes a [LogEvent] object and returns a list of values.
    #'
    #' @param transform_event_names A named `character` vector mapping original
    #'   field names to Dynatrace-compatible ones, or a function with a single
    #'   mandatory argument that accepts a character vector of field names.
    #'   Applied after to `transform_event()`.
    #'
    #' @param excluded_fields A `character` vector of field names to exclude
    #'   from the final output. Applied after `transform_event_names`.
    initialize = function(
      toJSON_args = list(auto_unbox = TRUE),
      timestamp_fmt = NULL,
      transform_event = function(event) event[["values"]],
      transform_event_names = identity,
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

      if (!is.null(self[["timestamp_fmt"]])){
        values[["timestamp"]] <- fmt_timestamp(values[["timestamp"]], self[["timestamp_fmt"]])
      }

      if (is.character(self$transform_event_names)){
        original_names <- names(values)
        rename_idx <- match(original_names, names(self$transform_event_names), nomatch = 0L)
        names(values)[rename_idx > 0L] <- self$transform_event_names[rename_idx[rename_idx > 0L]]

      } else if (is.function(self$transform_event_names)){
        names(values) <- self$transform_event_names(names(values))

      } else {
        warning("`transform_event_names` must be a character vector or a function")
      }

      if (!is.null(self$excluded_fields)) {
        values <- values[!names(values) %in% self$excluded_fields]
      }

      do.call(
        jsonlite::toJSON,
        args = c(list(x = values), get(".toJSON_args", private))
      )
    },

    set_toJSON_args = function(x){
      assert(is.list(x))
      assert(identical(length(names(x)), length(x)))
      private$.toJSON_args <- x
      invisible(self)
    },


    # . . setters -------------------------------------------------------------

    set_timestamp_fmt = function(x){
      assert(is.null(x) || is_scalar_character(x) || is.function(x))
      private[[".timestamp_fmt"]] <- x
      invisible(self)
    },

    set_transform_event = function(x){
      assert(
        is.function(x) &&
        identical(names(formals(x)), "event"),
        "`transform_event` must be a function a single argument `event`")

      private[[".transform_event"]] <- x
      invisible(self)
    },

    set_transform_event_names = function(x){
      assert(is.function(x) || is_field_name_map(x),
      "`transform_event_names` must be a named character vector or function with a single mandatory argument (optional arguments are OK)")

      private[[".transform_event_names"]] <- x
    },

    #  . . methods ----------------------------------------------------------------

    toString = function() {
      fmt_class(class(self)[[1]])
    },

    parse = function(
      file
      ){
        read_json_lines(file)
      },

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
    toJSON_args = function() {
      get(".toJSON_args", private)
    },

    timestamp_fmt = function() {
      get(".timestamp_fmt", private)
    },

    transform_event = function() get(".transform_event", private),

    transform_event_names = function() get(".transform_event_names", private)
  ),

  # . . private --------------------------------------------------------------
  private = list(
    .toJSON_args = NULL,
    .timestamp_fmt = NULL,
    .transform_event = NULL,
    .transform_event_names = NULL
  )
)


is_field_name_map <- function(x){
  is.character(x) && !is.null(names(x)) && all(nzchar(names(x)))
}
