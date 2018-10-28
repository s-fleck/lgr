
#' @include format.R
#' @include utils.R
#' @include utils-sfmisc.R
#'
#' @export
Appender <- R6::R6Class(
  "Appender",

  active = list(
    parent_memlog = function(value){
      if (missing(value)) private$.parent_memlog
      private$.parent_memlog <- value
    },

    threshold = function(value){
      if (missing(value)){
        res <- private$.threshold %||% private$.parent_memlog$threshold
        if (is.character(res))
          res <- private$.parent_memlog$unlabel_levels(res)
        return(res)
      }

      if (is.character(value))
        res <- private$.parent_memlog$unlabel_levels(res)

      is.null(value) || assert_valid_threshold(
        value,
        private$.parent_memlog$log_levels,
        sprintf("Illegal threshold set for appender <%s>: ", class(self)[[1]])
      )

      private$.threshold <- value
    }
  ),
    private = list(
      .threshold = NULL,
      .parent_memlog = NULL
  )
)




# appender format ---------------------------------------------------------

AppenderFormat <- R6::R6Class(
  "AppenderFormat",
  inherit = Appender,
  public = list(
    initialize = function(
      threshold = NULL,
      formatter = format.memlog_data,
      format = "%L [%t] %m",
      timestamp_format = "%Y-%m-%d %H:%M:%S",
      colors = NULL
    ){
      self$threshold <- threshold
      self$formatter <- formatter
      self$format <- format
      self$timestamp_format <- "%Y-%m-%d %H:%M:%S"
      self$colors <- colors
    }
  ),

  active = list(
    formatter = function(value){
      if (missing(value)) return(private$.formatter)
      private$.formatter <- value
    },

    format = function(value){
      if (missing(value)) return(private$.format)
      private$.format <- value
    },

    timestamp_format = function(value){
      if (missing(value)) return(private$.timestamp_format)
      private$.timestamp_format <- value
    },

    colors = function(value){
      if (missing(value)) return(private$.colors)
      private$.colors <- value
    }
  ),

  private = list(
    format_entry = function(x){
      private$.formatter(
        x,
        format = private$.format,
        timestamp_format = private$.timestamp_format,
        colors = private$.colors,
        ml = private$.parent_memlog
      )
    },
    .formatter = NULL,
    .format = NULL,
    .timestamp_format = NULL,
    .colors = NULL
  )
)




#' @export
AppenderConsole <- R6::R6Class(
  "AppenderConsole",
  inherit = AppenderFormat,
  public = list(
    append = function(){
      x <- private$.parent_memlog$collector$last_value

      if (x$level <= self$threshold)
        cat(private$format_entry(x), "\n")

      return(invisible(x$msg))
    }
  )
)




#' @inheritParams cat
#'
#' @export
AppenderFile <- R6::R6Class(
  "AppenderFile",
  inherit = AppenderFormat,
  public = list(
    initialize = function(
      file,
      threshold = NULL,
      formatter = format.memlog_data,
      format = "%L [%t] %m",
      timestamp_format = "%Y-%m-%d %H:%M:%S"
    ){
      self$file <- file
      self$threshold <- threshold
      self$formatter <- formatter
      self$format <- format
      self$timestamp_format <- format
    },


    append = function(){
      threshold <- self$threshold
      x <- private$.parent_memlog$collector$last_value

      if (x$level <= threshold){
        cat(
          private$format_entry(x), "\n",
          file = private$.file,
          append = TRUE
        )
      }

      return(invisible(x$msg))
    }
  ),

  active = list(
    file = function(value){
      if (missing(value)) return(private$.file)

      private$.file <- value
    }


  ),

  private = list(
    .file = NULL
  )
)




# appender glue -----------------------------------------------------------


AppenderGlue <- R6::R6Class(
  "AppenderGlue",
  inherit = AppenderFormat,
  public = list(
    initialize = function(
      threshold = NULL,
      format = "{toupper(.ml$label_levels(level))} [{format(timestamp, format = '%Y-%m-%d %H:%M:%S')}] {msg}",
      colors = NULL
    ){
      assert_namespace("glue")

      self$threshold <- threshold
      self$formatter <- glue::glue
      self$format <- format
      self$colors <- colors
    },

    append = function(){
      x <- private$.parent_memlog$collector$last_value
      if (x$level <= self$threshold){
        private$format_entry(x)
      } else {
        invisible(NULL)
      }
    }
  ),

  private = list(
    format_entry = function(x){
      .ml <- private$.parent_memlog  # make available for glue format
      do.call(glue::glue, c(list(private$.format), as.list(x)))
    }
  )
)




#' @export
AppenderConsoleGlue <- R6::R6Class(
  "AppenderConsoleGlue",
  inherit = AppenderGlue,
  public = list(
    append = function(){
      x <- private$.parent_memlog$collector$last_value

      if (x$level <= self$threshold)
        cat(private$format_entry(x), "\n")

      return(invisible(x$msg))
    }
  )
)




# appender minimal --------------------------------------------------------





#' @export
AppenderConsoleMinimal <- R6::R6Class(
  "AppenderConsoleMinimal",
  inherit = Appender,
  public = list(
    initialize = function(
      threshold = NULL,
      timestamp_format ="%Y-%m-%d %H:%M:%S"
    ){
      self$threshold <- threshold
      self$timestamp_format <- timestamp_format
    },


    append = function(){
      x <- private$.parent_memlog$collector$last_value

      if (x$level > self$threshold){
        return(invisible(x$msg))
      } else {

        cat(
          toupper(private$.parent_memlog$label_levels(x$level)),
          " [", format(x$timestamp, private$.timestamp_format), "] ",
          x$msg,
          "\n",
          sep = ""
        )
        return(invisible(x$msg))
      }
    }
  ),

  active = list(
    timestamp_format = function(value){
      assert(is_scalar_character(value))
      private$.timestamp_format <- value
    }
  ),

  private = list(
    .timestamp_format = NULL
  )
)



# appender crash ----------------------------------------------------------


# appender email ----------------------------------------------------------


