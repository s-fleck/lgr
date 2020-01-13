#' Abstract Class for Layouts
#'
#' Abstract classes are exported for developers that want to extend them, they
#' are not useful to casual users.  Layouts get a [LogEvent] passed down from an
#' [Appender], and format them for output. How this formatting works exactly
#' varies widely. For example for file or console output the log event is
#' usually formatted into a single character line.
#'
#' @section Creating a New Layout:
#'
#' Layouts are instantiated with `<LayoutSubclass>$new()`. For a description of
#' the arguments to this function please refer to the Fields section.
#'
#' @section Methods:
#'
#' \describe{
#'   \item{`format_event(event)`}{format a [LogEvent]}
#' }
#'
#'
#'
#' @name Layout
#' @aliases Layouts
#' @family Layouts
#' @include print_LogEvent.R
#' @include utils.R
#' @include utils-sfmisc.R
#' @include Filterable.R
#' @keywords internal
NULL




# Layout ------------------------------------------------------------------

#' @export
Layout <- R6::R6Class(
  "Layout",

  public = list(
    format_event = function(event) paste(capture.output(print(event$values)), collapse = " "),
    toString = function() "<empty>"
  )
)




# LayoutFormat ------------------------------------------------------------

#' Format Log Events as Text
#'
#' Format a [LogEvent] as human readable text using [format.LogEvent()], which
#' provides a quick and easy way to customize log messages. If you need
#' more control and flexibility, consider using [LayoutGlue] instead.
#'
#' @inheritSection Layout Methods
#' @inheritSection print.LogEvent Format Tokens
#'
#' @eval r6_usage(LayoutFormat)
#'
#' @section Creating a New LayoutFormat:
#'
#' A new LayoutFormat is instantiated with `LayoutFormat$new()`. For a
#' description of the arguments to this function please refer to the Fields,
#' and the documentation of [format.LogEvent()].
#'
#' @section Fields:
#' \describe{

#'  }
#'
#' @section Format Tokens:
#' This is the same list of format tokens as for [format.LogEvent()]
#'
#'
#' @name LayoutFormat
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
#'
NULL




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
      self$set_fmt(fmt)
      self$set_timestamp_fmt(timestamp_fmt)
      self$set_colors(colors)
      self$set_pad_levels(pad_levels)
    },

    #' @details Format a LogEvent
    #' @param event a [LogEvent]
    format_event = function(
      event
    ){
      format.LogEvent(
        event,
        fmt = private$.fmt,
        timestamp_fmt = private$.timestamp_fmt,
        colors = private$.colors,
        pad_levels = private$.pad_levels
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

    #' @details see Fields
    #' Convert Layout to a Character String
    toString = function(){
      paste(fmt_class(class(self)[[1]]), self$fmt)
    }
  ),


  active = list(
    #' @field fmt a `character` scalar containing format tokens. See [format.LogEvent()].
    fmt = function()  private$.fmt,

    #' @field timestamp_fmt a `character` scalar. See [base::format.POSIXct()].
    timestamp_fmt = function() private$.timestamp_fmt,

    #' @field colors a named `list` of functions (like the ones provided by
    #' the package [crayon]) passed on on [format.LogEvent()].
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
#' @eval r6_usage(LayoutGlue)
#'
#' @section Creating a New LayoutGlue:
#'
#' A new `LayoutGlue` is instantiated with `LayoutGlue$new()`. It takes a single
#' argument `fmt` that is passed on to `glue::glue()` for each LogEvent.
#'
#' @inheritSection Layout Methods
#' @section Fields:
#'
#' \describe{
#'   \item{`fmt`}{see [glue::glue()]}
#'  }
#'
#' @name LayoutGlue
#' @family Layouts
#' @include Filterable.R
#' @include log_levels.R
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
#'
NULL



#' @export
LayoutGlue <- R6::R6Class(
  "LayoutGlue",
  inherit = Layout,
  public = list(
    initialize = function(
      fmt = "{pad_right(colorize_levels(toupper(level_name)), 5)} [{timestamp}] msg"
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
    fmt = function()  private$.fmt
  ),

  private = list(
    .fmt = NULL
  )
)




# LayoutJson --------------------------------------------------------------

#' Format LogEvents as JSON
#'
#' Format a LogEvent as JSON
#'
#' @eval r6_usage(LayoutJson)
#'
#' @inheritSection Layout Creating a New Layout
#' @inheritSection Layout Methods
#'
#' @section Creating a New Layout:
#'
#' @section Fields:
#' \describe{
#'   \item{`toJSON_args`, `set_toJSON_args()`}{a list of values passed on to
#'     [jsonlite::toJSON()]
#'    }
#' }
#'
#' @section Methods:
#'
#' @name LayoutJson
#' @family Layouts
#' @include Filterable.R
#' @include log_levels.R
#' @seealso [read_json_lines()], [http://jsonlines.org/](http://jsonlines.org/)
#' @examples
#' # setup a dummy LogEvent
#'
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
#'
NULL




#' @export
LayoutJson <- R6::R6Class(
  "LayoutJson",
  inherit = Layout,
  public = list(
    initialize = function(
      toJSON_args = list(auto_unbox = TRUE)
    ){
      # init
      self$set_toJSON_args(toJSON_args)
    },

    format_event = function(event) {
      do.call(
        jsonlite::toJSON,
        args = c(list(x = event$values), get(".toJSON_args", private))
      )
    },

    set_toJSON_args = function(x){
      assert(is.list(x))
      assert(identical(length(names(x)), length(x)))
      private$.toJSON_args <- x
      invisible(self)
    },

    toString = function() {
      fmt_class(class(self)[[1]])
    }
  ),

  active = list(
    toJSON_args = function() private$.toJSON_args
  ),

  private = list(
    .toJSON_args = NULL
  )
)


