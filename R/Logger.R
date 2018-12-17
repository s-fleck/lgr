#' Loggers
#'
#' A Logger records the log message and some metadata (timestamp,
#' calling function) as a [LogEvent] and passes this event on to one or
#' several [Appenders] that write the event to a destination (a file,
#' the console, ...). All Loggers of an R session are in a hierarchical
#' structure, and each Logger (except the Root Looger) passes on LogEvents to
#' the Appenders of it ancestral Loggers. See `vignette("yog", package = "yog")`
#' for more info.
#'
#' @eval r6_usage(Logger)
#'
#' @section Creating Loggers:
#'
#' If you want logging for a Project (f.e a Package you are developing) that is
#' separate from the global logging, you can create a new logger with
#' `Logger$new()`. If you just want to add different outputs (for example
#' logfiles) to the Root Logger, look into [Appenders].
#'
#'
#' @section Fields:
#'
#' You can either specify the fields in `Logger$new()` or modify them after
#' creation with setter functions of the form `logger$set_fieldname(value)`
#' (see examples)
#'
#' \describe{
#'   \item{`name`}{`character` scalar. Name of the Logger. Should be unique amongst
#'     Loggers. If you define a logger for an R Package, the logger should have
#'     the same name as the Package.}
#'
#'   \item{`appenders`}{`list` of [Appender]s. The appenders used by this logger
#'     to write log entries to the console, to files, etc...}
#'
#'   \item{`threshold`}{`character` or `integer` scalar. The minimum log level
#'     that triggers this logger}
#'
#'   \item{`user`}{`character` scalar. The current user name or email adress.
#'     This information can be used by the appenders}
#'
#'   \item{`parent`}{a `Logger`. Usually the Root logger. All Loggers must be
#'     descentents of the Root logger for yog to work as intended.}
#'
#'   \item{`exception_handler`}{a `function` that takes a single argument `e`.
#'     The function used to handle errors that occur durring loging. Default
#'     to demoting any error to a [warning]}
#'
#'   \item{`propagate`}{`TRUE` or `FALSE`. Should log messages be passed on to
#'     the appenders of the ancestral Loggers?}
#'  }
#'
#'
#' @section Methods:
#'
#' \describe{
#'   \item{`fatal(msg, ...)`}{Logs a message with level `FATAL` on this logger.
#'     The arguments are interpreted as for `trace()`.}
#'
#'   \item{`error(msg, ...)`}{Logs a message with level `ERROR` on this logger.
#'     The arguments are interpreted as for `trace()`.}
#'
#'   \item{`warn(msg, ...)`}{Logs a message with level `WARN` on this logger.
#'     The arguments are interpreted as for `trace()`.}
#'
#'   \item{`info(msg, ...)`}{Logs a message with level `INFO` on this logger.
#'     The arguments are interpreted as for `trace()`.}
#'
#'   \item{`debug(msg, ...)`}{Logs a message with level `DEBUG` on this logger.
#'     The arguments are interpreted as for `trace()`.}
#'
#'   \item{`trace(msg, ...)`}{Logs a message with level `TRACE` on this logger.
#'   `msg` and `...` are passed on to [base::sprintf()].}
#'
#'   \item{`log(level, msg, timestamp, caller)`}{Logs a message with `level`.}
#'
#'   \item{`add_appender(appender, name = NULL)`}{Adds a new Appender to the
#'   Logger. `appender` must be an [Appender] object. `name` is optional and
#'   will be used as name in the list of appenders, i.e if you do
#'   `logger$add_appender(AppenderConsole$new(), name = "console")` you can
#'    refer to it via `logger$appenders$console`.}
#'
#'   \item{`remove_appender(pos)`}{Removes and Appender from a Logger. `pos`
#'   can be an `integer` or `character` vector referring either to the positions
#'   or names of the Appenders to be removed.}
#'
#'   \item{`exception_handler(e)`}{Used to handle errors that occur durring the
#'   logging process. The defaul is to convert errors to warnings.}
#'
#'   \item{`filter(x)`}{Determine whether the LogEvent `x` should be passed
#'   on to Appenders (`TRUE`) or not (`FALSE`). See also the active binding
#'   `filters`}
#'
#'   \item{`set_name(x)`}{Set the Logger name to the `character` scalar `x`}
#'
#' }
#'
#'
#' @section Fields:
#'
#' \describe{
#'
#'   \item{`appenders`}{A `list` of all Appenders attached to the current
#'     logger}
#'
#'   \item{`ancestry`}{*read only*. A `character` vector of the names of all
#'     Loggers that are ancestors to the current Logger}
#'
#'   \item{`inherited_appenders`}{*read only*. A `list` of all inherited
#'   appenders from ancestral Loggers of the current Logger}
#'
#'   \item{`filters`}{a `list` of predicates (functions that return either
#'   `TRUE` or `FALSE`). If all of these functions evaluate to `TRUE` the
#'   LogEvent is passed on to the Logger's Appenders}
#'
#'   \item{`last_event`}{The last LogEvent produced by the current Logger}
#'
#'   \item{`name`}{Name of the Logger. Mainly used for display purposes. If
#'     you define a Logger for a package, this should be the same name as
#'     the packages
#'   }
#'
#'   \item{`parent`}{Parent Logger of the current Logger (`NULL` for the Root Logger)}
#'
#'
#'   \item{`threshold`}{An `integer`. Threshold of the current Logger (i.e the
#'   maximum log level that this Logger processes)}
#'
#'   \item{`user`}{The current user}
#' }
#'
#' @name Logger
#' @aliases Loggers
#' @include Filterable.R
#' @include log_levels.R
#' @examples
#'
#' yog$info("Today is %s", Sys.Date() )
#'
#' # yog includes a pre-configured root logger
#' yog$fatal("This is a serious error")
#'
#' # if you want to take advantage of hierarchical logging, you can create new loggers.
#' # the following creates a new logger that logs to a temporary file.
#' tf <- tempfile()
#' mylogger <- Logger$new(
#'   "mylogger",
#'   appenders = AppenderFile$new(tf)
#' )
#'
#' # The new logger passes the log message on to the appenders of its parent
#' # logger, which is by default the root logger. This is why the following
#' # writes not only the file 'tf', but also to the console.
#' mylogger$fatal("blubb")
#' readLines(tf)
#'
NULL




#' @export
Logger <- R6::R6Class(
  "Logger",
  inherit = Filterable,
  cloneable = FALSE,

  # public --------------------------------------------------------------------
  public = list(

    # +- methods --------------------------------------------------------------
    initialize = function(
      name,
      appenders = list(),
      threshold = 400L,
      user = get_user(),
      parent = yog::yog,
      exception_handler = function(e){
        warning(
          "[", format(Sys.time(), format = "%Y-%m-%d %H:%M:%OS3"), "] ",
          "An error occured in the logging sub system: ", e
        )
      },
      propagate = TRUE
    ){
      # fields
      # threshold must be set *after* the logging functions have been initalized
      self$set_appenders(appenders)
      self$set_user(user)
      self$set_exception_handler(exception_handler)
      self$set_parent(parent)
      self$set_name(name)
      self$set_propagate(propagate)
      private$.last_event <- LogEvent$new(self)
      self$set_threshold(threshold)

      invisible(self)
    },


    finalize = function(){
      # ensure appenders are destroyed before logger is destroyed so that the
      # finalize method of the appenders can still access the logger if it
      # wants to
      for (i in rev(seq_along(self$appenders))){
        self$remove_appender(i)
      }

      gc()  # ensure that finalizers of appenders are executed now
      invisible()
    },


    log = function(
      level,
      msg,
      timestamp = Sys.time(),
      caller = get_caller(-3),
      ...
    ){
      force(caller)

      tryCatch({
        # preconditions
        if (is.character(level)) level <- unlabel_levels(level)
        assert_valid_log_levels(level)
        assert(
          identical(length(unique(level)), 1L),
          "Can only utilize vectorized logging if log level is the same for all entries"
        )

        # Check if LogEvent should be created
        if (
          identical(level[[1]] > private[[".threshold"]], TRUE) ||
          identical(getOption("yog.logging_suspended"), TRUE)
        ){
          return(invisible(msg))
        }


        # update log event
          # standard fields
          assign("level", level, envir = self[["last_event"]])
          assign("timestamp", timestamp,  envir = self[["last_event"]])
          assign("caller", caller, envir = self[["last_event"]])
          assign("msg", msg, envir = self[["last_event"]])

          # custom fields
          dots <- list(...)
          assert(
            identical(length(names(dots)), length(dots)),
            "Custom fields supplied to Logger must be named"
          )
          for (nm in names(dots)){
            assign(nm, dots[[nm]], envir = self[["last_event"]])
          }


        # emit
        if (self$filter(self$last_event)){
          for (app in c(self$appenders, self$inherited_appenders)) {
            if (app$filter(self$last_event)){
              app$append(self$last_event)
            }
          }
        }
      },
        error = self$handle_exception
      )
    },


    fatal = function(msg, ...){
      self$log(
        msg = sprintf(as.character(msg), ...),
        caller = get_caller(-4L),
        level = 100
      )
    },

    error = function(msg, ...){
      self$log(
        msg = sprintf(as.character(msg), ...),
        caller = get_caller(-4L),
        level = 200
      )
    },

    warn = function(msg, ...){
      self$log(
        msg = sprintf(as.character(msg), ...),
        caller = get_caller(-4L),
        level = 300
      )
    },

    info = function(msg, ...){
      self$log(
        msg = sprintf(as.character(msg), ...),
        caller = get_caller(-4L),
        level = 400
      )
    },

    debug = function(msg, ...){
      self$log(
        msg = sprintf(as.character(msg), ...),
        caller = get_caller(-4L),
        level = 500
      )
    },

    trace = function(msg, ...){
      self$log(
        msg = sprintf(as.character(msg), ...),
        caller = get_caller(-4L),
        level = 600
      )
    },

    add_appender = function(
      appender,
      name = NULL
    ){
      assert(inherits(appender, "Appender"))

      for(app in self$appenders){
        # appender is already attached to logger, nothing to do, this is
        # necessary because `logger$appender$blubb <- blagh` tries to readd the
        # whole appender list
        if (identical(app, appender)) return(invisible(self))
      }

      assert(
        is.null(appender$logger) || identical(appender$logger, self),
        "Cannot add appender to logger as it is already attached to another",
        "logger. Please create a new appender or clone the old one with",
        "`appender$clone()`, but be aware that this can cause weird bugs",
        "for some appender types."
        )

      private$.appenders[length(private$.appenders) + 1L] <- list(appender)

      if (!is.null(name))
        names(private$.appenders)[length(private$.appenders)] <- name

      invisible(self)
    },


    remove_appender = function(
      pos
    ){
      if (is.numeric(pos)){
        assert(
          all(pos %in% seq_along(private$.appenders)),
          "'pos' is out of range of the length of appenders (1:",
          length(private$.appenders), ")"
        )

        pos <- as.integer(pos)
      } else if (is.character(pos)) {
        assert(
          all(pos %in% names(private$.appenders)),
          "'pos' is not names of appenders (",
          paste(names(private$.appenders), collapse = ", "),
          ")"
        )
      }

      for (nm in pos){
        private$.appenders[[nm]] <- NULL
      }

      invisible(self)
    },

    handle_exception = function(...){
      private$.exception_handler(...)
    },

    set_name = function(x){
      assert(is_scalar_character(x))
      private$.name <- x
      invisible(self)
    },

    set_exception_handler = function(fun){
      assert(is.function(fun))
      private$.exception_handler <- fun
      invisible(self)
    },

    set_propagate = function(x){
      assert(is_scalar_bool(x))
      private$.propagate <- x
      invisible(self)
    },

    set_parent = function(logger){
      assert(is.null(logger) || inherits(logger, "Logger"))
      private$.parent <- logger
      invisible(self)
    },


    set_threshold = function(level){
      assert_valid_threshold(level)
      if (is_scalar_character(level))  level <- unlabel_levels(level)

      private$unsuspend()
      private$suspend(level, inclusive = FALSE)

      private$.threshold <- as.integer(level)
      invisible(self)
    },


    set_appenders = function(x){
      if (is.null(x)){
        private$.appenders <- list()
        return(invisible())
      }
      if (inherits(x, "Appender"))  x <- list(x)

      assert(
        is.list(x) && all(vapply(x, inherits, TRUE, "Appender")),
        "'appenders' must either be a single Appender, a list thereof, or ",
        "NULL for no appenders."
      )
      for (i in seq_along(x))  self$add_appender(x[[i]], name = names(x)[[i]])

      invisible(self)
    },

    set_user = function(x){
      assert(is_scalar_character(x), "'user' must be a scalar character")
      private$.user <- x
      invisible(self)
    }
  ),

  # active bindings ---------------------------------------------------------
  active = list(
    name = function() private$.name,

    propagate = function() private$.propagate,

    last_event = function() private$.last_event,

    ancestry = function(){
      structure(
        c(self$name, private$.parent$ancestry),
        class = c("ancestry", "character")
      )
    },

    parent = function() private$.parent,

    threshold = function() private$.threshold,

    inherited_appenders = function(){
      if (self$propagate){
        c(
          private$.parent$appenders,
          private$.parent$inherited_appenders
        )
      } else {
        NULL
      }
    },

    exception_handler = function() private$.exception_handler,

    appenders = function()  private$.appenders,

    user = function() private$.user
  ),


  # private -----------------------------------------------------------------
  private = list(

    # +- methods --------------------------------------------------------------
    suspend = function(
      threshold = 0,
      inclusive = TRUE
    ){
      assert(is_scalar_bool(inclusive))
      assert(is_valid_log_levels(threshold))
      if (is.na(threshold))
        threshold <- Inf

      if (is.character(threshold))
        threshold <- unlabel_levels(threshold)

      assert(is.numeric(threshold))

      if (inclusive){
        ll <- private$.log_levels[private$.log_levels >= threshold]
      } else {
        ll <- private$.log_levels[private$.log_levels > threshold]
      }

      for (i in seq_along(ll)){
        nm  <- names(ll)[[i]]
        lvl <- ll[[i]]
        private$suspended_loggers[[nm]] <- self[[nm]]
        unlockBinding(nm, env = self)
        self[[nm]] <- function(...) invisible()
        lockBinding(nm, env = self)
      }
      invisible(self)
    },


    unsuspend = function(
      threshold = 0,
      inclusive = TRUE
    ){
      assert(is_scalar_bool(inclusive))
      assert(is_valid_log_levels(threshold, private$.log_levels))
      if (is.na(threshold))
        threshold <- Inf

      if (is.character(threshold))
        threshold <- unlabel_levels(threshold, private$.log_levels)

      assert(is.numeric(threshold))

      if (inclusive){
        ll <- private$.log_levels[private$.log_levels >= threshold]
      } else {
        ll <- private$.log_levels[private$.log_levels > threshold]
      }

      if (is.na(threshold)) threshold <- Inf
      ll <- private$.log_levels[private$.log_levels >= threshold]

      for (i in seq_along(private$suspended_loggers)){
        nm  <- names(private$suspended_loggers)[[i]]
        unlockBinding(nm, env = self)
        self[[nm]] <- private$suspended_loggers[[nm]]
        lockBinding(nm, env = self)
      }
      private$suspended_loggers <- list()
      invisible(self)
    },


    # +- fields ---------------------------------------------------------------
    .propagate = NULL,
    .filters = list(check_threshold),
    .exception_handler = NULL,
    .name = NULL,
    .parent = NULL,
    .appenders = NULL,
    .user = NA_character_,
    .threshold = NA_integer_,
    .last_event = NULL,

    # intentionaly hardcoded and not using the global options. this is used to
    # track which logging functions are available for the logger. used by
    # suspend/unsuspend
    .log_levels = as_log_levels(c(
      "fatal" = 100L,
      "error" = 200L,
      "warn"  = 300L,
      "info"  = 400L,
      "debug" = 500L,
      "trace" = 600L
    )),
    suspended_loggers = list()
  )
)




# utils -------------------------------------------------------------------

# Given a string, indent every line by some number of spaces.
# The exception is to not add spaces after a trailing \n.
indent <- function(str, indent = 0) {
  gsub("(^|\\n)(?!$)",
       paste0("\\1", paste(rep(" ", indent), collapse = "")),
       str,
       perl = TRUE
  )
}




# Trim a string to n characters; if it's longer than n, add " ..." to the end
trim <- function(str, n = 60) {
  if (nchar(str) > n) paste(substr(str, 1, n-4), "...")
  else str
}
