
#' @include format.R
#' @include utils.R
#' @include utils-sfmisc.R
#'
#' @export
appender <- R6::R6Class(
  "appender",
  public = list(append = function() NULL )
)




#' @export
appender_console <- function(
  x,
  ...,
  ml
){
  cat(ml$format_data(x, ..., ml = ml))
}




#' @export
appender_file <- R6::R6Class(
  "appender_file",
  inherit = appender,
  public = list(
    append = NULL,
    file = NULL,
    format = NULL
  )
)




#' @export
appender_console <- R6::R6Class(
  "appender_console",
  inherit = appender,
  public = list(
    initialize = function(
      threshold = NULL,
      formatter = format.memlog_data,
      format = "%L [%t] %m",
      timestamp_format = "%Y-%m-%d %H:%M:%S",
      colors = NULL
    ){
      assert(
        is.null(threshold) ||
        is_scalar_integerish(threshold) ||
        is_scalar_character(threshold)
      )

      self$formatter <- formatter
      self$format <- format
      self$timestamp_format <- timestamp_format
      self$colors <- colors
      self$threshold <- threshold
    },

    append = function(ml){
      threshold <- self$get_threshold(ml)
      x <- ml$get_last_row()

      if (x$level > threshold){
        return(invisible(x$msg))
      } else {
        dd <- self$formatter(
          x,
          format = self$format,
          timestamp_format = self$timestamp_format,
          colors = self$colors,
          ml = ml
        )
      }
      cat(dd, "\n")
      return(invisible(x$msg))
    },

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
    threshold = NULL,
    formatter = NULL,
    format = NULL,
    timestamp_format = NULL,
    colors = NULL
  )
)




#' @export
appender_console_minimal <- R6::R6Class(
  "appender_console",
  inherit = appender,
  public = list(
    initialize = function(
      threshold = NULL,
      timestamp_format ="%Y-%m-%d %H:%M:%S"
    ){
      assert(
        is.null(threshold) ||
        is_scalar_integerish(threshold) ||
        is_scalar_character(threshold)
      )

      self$timestamp_format <- timestamp_format
      self$threshold <- threshold
    },

    append = function(ml){
      threshold <- self$get_threshold(ml)
      x <- ml$get_last_row()

      if (x[[2]] > threshold){
        return(invisible(x$msg))
      } else {
        cat(
          toupper(ml$label_levels(x[[2]])),
          " [", format(x[[3]], self$timestamp_format), "] ",
          x[[5]],
          "\n",
          sep = ""
        )
        return(invisible(x$msg))
      }
    },

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
    threshold = NULL,
    formatter = NULL,
    format = NULL,
    timestamp_format = NULL,
    colors = NULL
  )
)




#' @export
console_appender_minimal <- appender_console_minimal$new()




#' @export
console_appender_color <- appender_console$new(
  colors = list(
    "fatal" = function(x) colt::clt_error(colt::clt_emph2(x)),
    "error" = colt::clt_error,
    "warn"  = colt::clt_warning,
    "debug" = colt::clt_chr,
    "trace" = colt::clt_chr_subtle
  )
)
