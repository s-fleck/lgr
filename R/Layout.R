#' @include print.R
#' @include utils.R
#' @include utils-sfmisc.R

# Layout ------------------------------------------------------------------

#' @export
Layout <- R6::R6Class(
  "Layout",

  public = list(
    format_event = function(x) paste(capture.output(print(x$values)), collapse = " ")
  ),

  private = list(
    formatter = NULL
  )
)



# LayoutFormat ------------------------------------------------------------

#' @export
LayoutFormat <- R6::R6Class(
  "LayoutFormat",
  inherit = Layout,
  public = list(
    initialize = function(
      fmt = "%L [%t] %m",
      timestamp_fmt = "%Y-%m-%d %H:%M:%OS3",
      colors = NULL,
      pad_levels = "right"
    ){
      private$formatter <- format.yog_data
      self$fmt <- fmt
      self$timestamp_fmt <- timestamp_fmt
      self$colors <- colors
      self$pad_levels <- pad_levels
    },

    format_event = function(
      x
    ){
      private[["formatter"]](
        x,
        fmt = private$.fmt,
        timestamp_fmt = private$.timestamp_fmt,
        colors = private$.colors,
        pad_levels = private$.pad_levels
      )
    }
  ),

  active = list(
    fmt = function(value){
      if (missing(value)) return(private$.fmt)
      assert(is_scalar_character(value))
      private$.fmt <- value
    },

    timestamp_fmt = function(value){
      if (missing(value)) return(private$.timestamp_fmt)
      assert(is_scalar_character(value))
      private$.timestamp_fmt <- value
    },

    colors = function(value){
      if (missing(value)) return(private$.colors)
      assert(
        is.null(value) || is.list(value),
        "'colors' must either be NULL or a list of functions, not ",
        class_fmt(value)
      )
      private$.colors <- value
    },

    pad_levels = function(value){
      if (missing(value)) return(private$.pad_levels)
      assert(is_scalar_character(value))
      private$.pad_levels <- value
    }
  ),

  private = list(
    .fmt = NULL,
    .timestamp_fmt = NULL,
    .colors = NULL,
    .pad_levels = NULL
  )
)




# LayoutJson --------------------------------------------------------------

#' @seealso http://jsonlines.org/
#' @export
LayoutJson <- R6::R6Class(
  "LayoutJson",
  inherit = Layout,
  public = list(
    initialize = function(
      event_vals  = c("level", "timestamp", "caller", "msg"),
      logger_vals = NULL,
      toJSON_args = list(auto_unbox = TRUE)
    ){
      self$toJSON_args <- toJSON_args
      self$event_vals <- event_vals
      self$logger_vals <- logger_vals
    },

    format_event = function(x) {
      vals <- mget(self$event_vals, x)

      if (!is.null(self$logger_vals)){
        vals <- c(vals, mget(self$logger_vals, x[["logger"]]))
      }

      do.call(jsonlite::toJSON, args = c(list(vals), self$toJSON_args))
    },

    toJSON_args = NULL,
    event_vals = NULL,
    logger_vals = NULL
  ),

  active = list(
    pad_levels = function(value){
      if (missing(value)) return(private$.pad_levels)
      assert(is_scalar_character(value))
      private$.pad_levels <- value
    }
  ),

  private = list(
    .toJSON_args = NULL
  )
)



# LayoutGlue --------------------------------------------------------------

#' @export
LayoutGlue <- R6::R6Class(
  "Layout",
  inherit = LayoutFormat,
  public = list(
    initialize = function(
      fmt = "{toupper(label_levels(level, x$logger$levels))} [{strftime(timestamp, format = '%Y-%m-%d %H:%M:%S')}] {msg}"
    ){
      private$formatter <- glue::glue
      self$fmt <- fmt
    },

    format_event = function(x) {
      do.call(
        private[["formatter"]],
        c(list(private$.fmt), x)
      )
    }
  ),

  active = list(
    fmt = function(value){
      if (missing(value)) return(private$.fmt)
      assert(is_scalar_character(value))
      private$.fmt <- value
    }
  ),

  private = list(
    .fmt = NULL,
    .timestamp_fmt = NULL,
    .colors = NULL,
    .pad_levels = NULL
  )
)
