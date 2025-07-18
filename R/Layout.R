# Layout ------------------------------------------------------------------


#' Abstract Class for Layouts
#'
#' [Appenders] pass [LogEvents][LogEvent] to a Layout which formats it for
#' output. For the Layouts included in lgr that means turning the LogEvent
#' into a `character` string.
#'
#' For each Appender exist one more more possible Layouts, but not every Layout
#' will work with every Appender. See the package \pkg{lgrExtra} for examples
#' for Layouts that return different data types (such as `data.frames`) and
#' Appenders that can handle them.
#'
#' @section Notes for developers:
#' Layouts may have an additional `$read(file, threshold, n)` method that returns
#' a `character` vector, and/or an `$parse(file)` method that
#' returns a `data.frame`. These can be used by Appenders to `$show()` methods
#' and `$data` active bindings respectively (see source code of [AppenderFile]).
#'
#' @aliases Layouts
#' @family Layouts
#' @include LogEvent.R
#' @include utils.R
#' @include utils-sfmisc.R
#' @include Filterable.R
#' @include log_levels.R
#'
#' @export
Layout <- R6::R6Class(
  "Layout",

  public = list(

    #' @description Format a log event
    #'
    #' Function that the Layout uses to transform a [LogEvent] into something
    #' that an [Appender] can write to an output destination.
    #'
    #' @param event a [LogEvent]
    format_event = function(event){
      toString(event)
    },

    toString = function() fmt_class(class(self)[[1]]),

    # . . setters -----------------------------------------------------------------
    set_excluded_fields = function(x){
      assert(is.null(x) || is.character(x))
      private$.excluded_fields <- x
      invisible(self)
    }
  ),


  # . . active --------------------------------------------------------------
  active = list(
    #' @field excluded_fields fields to exclude from the final log
    excluded_fields = function() {
      get(".excluded_fields", private)
    }
  ),


  # . . private -------------------------------------------------------------
  private = list(
    .excluded_fields = NULL
  )
)




# LayoutFormat ------------------------------------------------------------

#' Format Log Events as Text
#'
#' Format a [LogEvent] as human readable text using [format.LogEvent()], which
#' provides a quick and easy way to customize log messages. If you need
#' more control and flexibility, consider using [LayoutGlue] instead.
#'
#' @inheritSection print.LogEvent Format Tokens
#'
#' @section Format Tokens:
#' This is the same list of format tokens as for [format.LogEvent()]
#'
#'
#' @export
#' @family Layouts
#' @include Filterable.R
#' @include log_levels.R
#' @examples
#' # setup a dummy LogEvent
#' event <- LogEvent$new(
#'   logger = Logger$new("dummy logger"),
#'   level = 200,
#'   timestamp = Sys.time(),
#'   caller = NA_character_,
#'   msg = "a test message"
#' )
#' lo <- LayoutFormat$new()
#' lo$format_event(event)
LayoutFormat <- R6::R6Class(
  "LayoutFormat",
  inherit = Layout,
  public = list(
    initialize = function(
      fmt = "%L [%t] %m %j",
      timestamp_fmt = "%Y-%m-%d %H:%M:%OS3",
      colors = NULL,
      pad_levels = "right",
      excluded_fields = NULL
    ){
      self$set_fmt(fmt)
      self$set_timestamp_fmt(timestamp_fmt)
      self$set_colors(colors)
      self$set_pad_levels(pad_levels)
      self$set_excluded_fields(excluded_fields)
    },

    #' @description Format a LogEvent
    #' @param event a [LogEvent]
    format_event = function(
      event
    ){
      format.LogEvent(
        event,
        fmt = private$.fmt,
        timestamp_fmt = private$.timestamp_fmt,
        colors = private$.colors,
        pad_levels = private$.pad_levels,
        excluded_fields = private$.excluded_fields
      )
    },

    #' @details see Fields
    set_fmt = function(x){
      assert(is_scalar_character(x))
      private$.fmt <- x
      invisible(self)
    },

    #' @details see Fields
    set_timestamp_fmt = function(x){
      assert(is_scalar_character(x))
      private$.timestamp_fmt <- x
      invisible(self)
    },

    #' @details see Fields
    set_colors = function(x){
      assert(
        is.null(x) || is.list(x),
        "'colors' must either be NULL or a list of functions, not ",
        class_fmt(x)
      )
      private$.colors <- x
      invisible(self)
    },

    #' @details see Fields
    set_pad_levels = function(x){
      assert(is_scalar_character(x))
      private$.pad_levels <- x
      invisible(self)
    },

    #' @details
    #' Convert Layout to a `character` string
    toString = function(){
      paste(fmt_class(class(self)[[1]]), self$fmt)
    },

    #' Read a log file written using LayoutFormat
    #' @param threshold a `character` or `integer` threshold
    #' @param n number of log entries to display
    read = function(
      file,
      threshold = NA_integer_,
      n = 20L
    ){
      assert(is_scalar_integerish(n))
      threshold_ori <- threshold
      threshold <- standardize_threshold(threshold)

      dd <- readLines(file)
      sel <- TRUE

      if (!is.na(threshold)){
        lvls_keep <- get_log_levels()[get_log_levels() <= threshold]

        if (grepl("%L", self$fmt, ignore.case = TRUE)){
          sel <- grep(
            paste0("(", names(lvls_keep), ")", collapse = "|"),
            dd,
            ignore.case = TRUE
          )

        } else if (grepl("%n", self$fmt)){
          sel <- grep(paste0("(", lvls_keep, ")", collapse = "|"), dd)

        } else {
          warning(sprintf(paste(
            "A threshold of `%s` was but the Layout's format specification",
            "('%s') does not support filtering by log level."
          )), threshold_ori, self$fmt )
        }
      }

      dd <- tail(dd[sel], n)
      dd
    }
  ),


  active = list(
    #' @field fmt a `character` scalar containing format tokens. See [format.LogEvent()].
    fmt = function()  private$.fmt,

    #' @field timestamp_fmt a `character` scalar. See [base::format.POSIXct()].
    timestamp_fmt = function() private$.timestamp_fmt,

    #' @field colors a named `list` of functions (like the ones provided by
    #' the package \pkg{crayon}) passed on on [format.LogEvent()].
    colors = function() private$.colors,

    #' @field pad_levels `"right"`, `"left"` or `NULL`. See [format.LogEvent()].
    pad_levels = function() private$.pad_levels
  ),

  private = list(
    .fmt = NULL,
    .timestamp_fmt = NULL,
    .colors = NULL,
    .pad_levels = NULL
  )
)




# LayoutGlue ------------------------------------------------------------

#' Format Log Events as Text via glue
#'
#' Format a [LogEvent] as human readable text using [glue::glue]. The function
#' is evaluated in an environment in which it has access to all elements of
#' the [LogEvent] (see examples). This is more flexible than [LayoutFormat],
#' but also more complex and slightly less performant.
#'
#' @export
#' @family Layouts
#' @seealso lgr exports a number of formatting utility functions that are
#'   useful for layout glue: [colorize_levels()], [pad_left()], [pad_right()].
#' @examples
#' lg <- get_logger("test")$
#'   set_appenders(AppenderConsole$new())$
#'   set_propagate(FALSE)
#'
#' lg$appenders[[1]]$set_layout(LayoutGlue$new())
#' lg$fatal("test")
#'
#'
#' # All fields of the LogEvent are available, even custom ones
#' lg$appenders[[1]]$layout$set_fmt(
#'   "{logger} {level_name}({level}) {caller}: {toupper(msg)} {{custom: {custom}}}"
#' )
#' lg$fatal("test", custom = "foobar")
#' lg$config(NULL)  # reset logger config
LayoutGlue <- R6::R6Class(
  "LayoutGlue",
  inherit = Layout,
  public = list(
    initialize = function(
      fmt = "{pad_right(colorize_levels(toupper(level_name)), 5)} [{timestamp}] {msg}"
    ){
      assert_namespace("glue")
      self$set_fmt(fmt)
    },


    format_event = function(
      event
    ){
      op <- parent.env(event)
      on.exit(parent.env(event) <- op)
      parent.env(event) <- environment()
      unclass(glue::glue(get(".fmt", private), .envir = event))
    },


    set_fmt = function(x){
      assert(is_scalar_character(x))
      private$.fmt <- x
      invisible(self)
    },


    set_colors = function(x){
      assert(
        is.null(x) || is.list(x),
        "'colors' must either be NULL or a list of functions, not ",
        class_fmt(x)
      )
      private$.colors <- x
      invisible(self)
    },


    toString = function() {
      paste(fmt_class(class(self)[[1]]), self$fmt)
    }
  ),


  active = list(
    #' @field fmt A string that will be interpreted by [glue::glue()]
    fmt = function()  private$.fmt
  ),

  private = list(
    .fmt = NULL
  )
)







# utils -------------------------------------------------------------------

fmt_timestamp = function(x, fmt){
  if (is.character(fmt)){
    format(x, fmt)
  } else if (is.function(fmt)){
    fmt(x)
  }
}
