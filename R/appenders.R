
#' @include format.R
#' @include utils.R
#' @include utils-sfmisc.R
#'
#' @export
appender <- R6::R6Class(
  "appender",
  public = list(
    get_threshold = function(
      ml
    ){
      threshold <- self$threshold %||% ml$get_threshold()
      if(is.character(threshold)){
        ml$unlabel_levels(threshold)
      }
      assert_valid_threshold(
        threshold,
        ml$get_log_levels(),
        sprintf("Illegal threshold set for appender <%s>: ", class(self)[[1]])
      )
      threshold
    },

    set_threshold = function(x){
      is.null(x) || assert_valid_threshold(x)
      private$threshold <- x
    }

  ),
  private = list(
    threshold = NULL
  )
)




# appender format ---------------------------------------------------------

appender_format <- R6::R6Class(
  "appender_format",
  inherit = appender,
  public = list(
    initialize = function(
      threshold = NULL,
      formatter = format.memlog_data,
      format = "%L [%t] %m",
      timestamp_format = "%Y-%m-%d %H:%M:%S",
      colors = NULL
    ){
      private$threshold <- threshold
      private$formatter <- formatter
      private$format <- format
      private$timestamp_format <- "%Y-%m-%d %H:%M:%S"
      private$colors <- colors
    }
  ),
  private = list(
    format_entry = function(x, ml){
      private$formatter(
        x,
        format = private$format,
        timestamp_format = private$timestamp_format,
        colors = private$colors,
        ml = ml
      )
    },
    formatter = NULL,
    format = NULL,
    timestamp_format = NULL,
    colors = NULL
  )
)




#' @export
appender_console <- R6::R6Class(
  "appender_console",
  inherit = appender_format,
  public = list(
    append = function(ml){
      threshold <- self$get_threshold(ml)
      x <- ml$get_collector()$get_last_value()

      if (x$level <= threshold){
        cat(private$format_entry(x, ml), "\n")

      } else {
        return(invisible(x$msg))
      }
      return(invisible(x$msg))
    }
  )
)




#' @inheritParams cat
#'
#' @export
appender_file <- R6::R6Class(
  "appender_file",
  inherit = appender_format,
  public = list(
    initialize = function(
      file,
      threshold = NULL,
      formatter = format.memlog_data,
      format = "%L [%t] %m",
      timestamp_format = "%Y-%m-%d %H:%M:%S"
    ){
      self$set_file(file)
      private$threshold <- threshold
      private$formatter <- formatter
      private$format <- "%L [%t] %m"
      private$timestamp_format <- "%Y-%m-%d %H:%M:%S"
    },

    set_file = function(x){
      private$file <- x
    },

    append = function(ml){
      threshold <- self$get_threshold(ml)
      x <- ml$get_collector()$get_last_value()

      if (x$level <= threshold){
        cat(private$format_entry(x, ml), "\n", file = private$file, append = TRUE)

      } else {
        return(invisible(x$msg))
      }
      return(invisible(x$msg))
    }
  ),
  private = list(
    file = NULL
  )
)




# appender glue -----------------------------------------------------------


appender_glue <- R6::R6Class(
  "appender_glue",
  inherit = appender,
  public = list(
    initialize = function(
      threshold = NULL,
      format = "{toupper(ml$label_levels(level))} [{format(timestamp, format = '%Y-%m-%d %H:%M:%S')}] {msg}",
      colors = NULL
    ){
      assert_namespace("glue")
      private$threshold <- threshold
      private$format <- format
      private$colors <- colors
    },
    append = function(ml){
      threshold <- self$get_threshold(ml)
      x <- ml$get_collector()$get_last_value()
      private$format_entry(x, ml)
    }
  ),
  private = list(
    format_entry = function(x, ml){
      do.call(glue::glue, c(list(private$format), as.list(x)))
    },
    formatter = NULL,
    format = NULL,
    timestamp_format = NULL,
    colors = NULL
  )
)




#' @export
appender_console_glue <- R6::R6Class(
  "appender_console_glue",
  inherit = appender_glue,
  public = list(
    append = function(ml){
      threshold <- self$get_threshold(ml)
      x <- ml$get_collector()$get_last_value()

      if (x$level <= threshold){
        cat(private$format_entry(x, ml), "\n")

      } else {
        return(invisible(x$msg))
      }
      return(invisible(x$msg))
    }
  )
)




# appender minimal --------------------------------------------------------





#' @export
appender_console_minimal <- R6::R6Class(
  "appender",
  inherit = appender,
  public = list(
    initialize = function(
      threshold = NULL,
      timestamp_format ="%Y-%m-%d %H:%M:%S"
    ){
      self$set_threshold(threshold)
      self$set_timestamp_format(timestamp_format)
    },

    set_timestamp_format = function(x){
      assert(is_scalar_character(x))
      private$timestamp_format <- x
    },

    append = function(ml){
      threshold <- self$get_threshold(ml)
      x <- ml$get_collector()$get_last_value()

      if (x$level > threshold){
        return(invisible(x$msg))
      } else {

        cat(
          toupper(ml$label_levels(x$level)),
          " [", format(x$timestamp, private$timestamp_format), "] ",
          x$msg,
          "\n",
          sep = ""
        )
        return(invisible(x$msg))
      }
    }
  ),
  private = list(
    timestamp_format = NULL
  )
)
