#' @include print.R
#' @include utils.R
#' @include utils-sfmisc.R

# Layout ------------------------------------------------------------------

#' @export
Layout <- R6::R6Class(
  "Layout",

  public = list(
    format_event = function(x) paste(capture.output(print(x)), collapse = " ")
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
      timestamp_fmt = "%Y-%m-%d %H:%M:%S",
      colors = NULL,
      log_levels = c(
        "fatal" = 1,
        "error" = 2,
        "warn"  = 3,
        "info"  = 4,
        "debug" = 5,
        "trace" = 6
      ),
      pad_levels = "right"
    ){
      private$formatter <- format.yog_data
      self$fmt <- fmt
      self$timestamp_fmt <- timestamp_fmt
      self$colors <- colors
      self$log_levels <- log_levels
      self$pad_levels <- pad_levels
    },

    format_event = function(x) {
      private[["formatter"]](
        x,
        fmt = private$.fmt,
        timestamp_fmt = private$.timestamp_fmt,
        colors = private$.colors,
        log_levels = private$.log_levels,
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
      assert(is.null(value) || is.list(value))
      private$.colors <- value
    },

    log_levels = function(value){
      if (missing(value)) return(private$.log_levels)
      assert(is.null(value) || is_integerish(value))
      value <- setNames(as.integer(value), names(value))
      private$.log_levels <- value
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
    .log_levels = NULL,
    .pad_levels = NULL
  )
)




# LayoutGlue --------------------------------------------------------------

#' @export
LayoutGlue <- R6::R6Class(
  "Layout",
  inherit = LayoutFormat,
  public = list(
    initialize = function(
      fmt = "{toupper(label_levels(level, self$log_levels))} [{strftime(timestamp, format = '%Y-%m-%d %H:%M:%S')}] {msg}",
      log_levels = c(
        "fatal" = 1,
        "error" = 2,
        "warn"  = 3,
        "info"  = 4,
        "debug" = 5,
        "trace" = 6
      )
    ){
      private$formatter <- glue::glue
      self$fmt <- fmt
      self$log_levels <- log_levels
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
    },

    log_levels = function(value){
      if (missing(value)) return(private$.log_levels)
      assert(is.null(value) || is_integerish(value))
      value <- setNames(as.integer(value), names(value))
      private$.log_levels <- value
    }
  ),

  private = list(
    .fmt = NULL,
    .timestamp_fmt = NULL,
    .colors = NULL,
    .log_levels = NULL,
    .pad_levels = NULL
  )
)
