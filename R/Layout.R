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




# LayoutJSON --------------------------------------------------------------

#' LayoutJSON
#'
#' Format an LogEvent as JSON
#'
#' @section Usage:
#'
#' ```
#' lo <- LayoutJSON$new()
#'
#' # methods
#'  lo$format_event(x)
#'
#' # fields / active bindings
#'  lo$event_vals
#'  lo$logger_vals
#'  lo$other_vals
#'  lo$toJSON_args
#'
#' ```
#'
#' @section Creating a new LayoutJSON:
#'
#' If you want logging for a Project (f.e a Package you are developing) that is
#' separate from the global logging, you can create a new logger with
#' `Logger$new()`. If you just want to add different outputs (for example
#' logfiles) to the root logger, look into [Appenders].
#'
#' \describe{
#'   \item{name}{`character` scalar. Name of the Logger. Should be unique amongst
#'     Loggers. If you define a logger for an R Package, the logger should have
#'     the same name as the Package.}
#'   \item{appenders}{`list` of [Appender]s. The appenders used by this logger
#'     to write log entries to the console, to files, etc...}
#'   \item{threshold}{`character` or `integer` scalar. The minimum log level
#'     that triggers this logger}
#'   \item{user}{`character` scalar. The current user name or email adress.
#'     This information can be used by the appenders}
#'   \item{parent}{a `Logger`. Usually the Root logger. All Loggers must be
#'     descentents of the Root logger for yog to work as intended.}
#'
#'   \item{string_formatter}{a `function` used to format the log strings passed
#'     to the logging functions (`fatal()`, `error()`, etc...). Defaults to
#'     [base::sprintf()]. Another sensible choice wuld be [glue::glue()].}
#'
#'   \item{handle_exception}{a `function` that takes a single argument `e`.
#'     The function used to handle errors that occur durring loging. Default
#'     to demoting any error to a [warning]}
#'  }
#'
#'
#' @section Methods:
#'
#' \describe{
#'   \item{`format_event(x)`}{format a LogEvent}
#'
#'   \item{`event_vals`}{Names of the fields of the event (i.e the LogEvent)
#'     to include in the resulting JSON object.
#'   }
#'
#'   \item{`logger_vals`}{Names of the fields of the Logger that produced the
#'     record to include in the resulting JSON object.
#'   }
#'
#'   \item{`other_vals`}{A named `list` of any kind of R value that can be
#'     serialized to Json. Functions in this `list`` will be executed with no
#'     arguments and their results will be included in the results object
#'     (see examples)
#'    }
#' }
#'
#'
#' @name LayoutJSON
#' @include Filterable.R
#' @include log_levels.R
#' @seealso [read_json_lines()], [http://jsonlines.org/](http://jsonlines.org/)
#' @examples
#'
#' # setup a dummy LogEvent
#' event <- LogEvent$new(
#'   logger = Logger$new("dummy logger", user = "testuser"),
#'   level = 200,
#'   timestamp = Sys.time(),
#'   caller = NA_character_,
#'   msg = "a test message"
#' )
#' lo <- LayoutJSON$new(
#'   event_vals = c("level", "timestamp", "msg"),
#'   logger_vals = "user",
#'   other_vals = list(pid = Sys.getpid, random_number = function() runif(3), teststring = "blah")
#' )
#' lo$format_event(event)
#' lo$format_event(event)
#'
NULL


#' @export
LayoutJSON <- R6::R6Class(
  "LayoutJSON",
  inherit = Layout,
  public = list(
    initialize = function(
      event_vals  = c("level", "timestamp", "caller", "msg"),
      logger_vals = NULL,
      other_vals = NULL,
      toJSON_args = list(auto_unbox = TRUE)
    ){
      self$toJSON_args <- toJSON_args
      self$event_vals  <- event_vals
      self$logger_vals <- logger_vals
      self$other_vals  <- other_vals
    },

    format_event = function(x) {
      vals <- mget(self$event_vals, x)

      if (!is.null(self$logger_vals)){
        vals <- c(vals, mget(self$logger_vals, x[["logger"]]))
      }

      if (!is.null(private$.other_vals)){
        ov <- private$.other_vals
        for (i in seq_along(ov)){
         nm <- names(ov)[[i]]
         if (is.function(ov[[i]]))
           vals[[nm]] <- ov[[i]]()
         else
           vals[[nm]] <- ov[[i]]
        }
      }

      do.call(jsonlite::toJSON, args = c(list(vals), self$toJSON_args))
    },

    toJSON_args = NULL,
    event_vals = NULL,
    logger_vals = NULL
  ),

  active = list(
    other_vals = function(value){
      if (missing(value)) return(private$.other_vals)
      assert(is.list(value))
      assert(identical(length(names(value)), length(value)))
      private$.other_vals <- value
    }
  ),

  private = list(
    .toJSON_args = NULL,
    .other_vals = NULL
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
