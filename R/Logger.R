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
#'     The function used to handle errors that occur durring loging. Defaults
#'     to demoting errors to [warnings]}
#'
#'   \item{`propagate`}{`TRUE` or `FALSE`. Should log messages be passed on to
#'     the appenders of the ancestral Loggers?}
#'  }
#'
#'
#' @section Methods:
#'
#' \describe{
#'   \item{`fatal(msg, ...)`}{Logs a message with level `fatal` on this logger.
#'     If there are *unnamed* arguments in `...`, they will be pased to
#'     `base::sprintf()` along with message. Named arguments will be passed
#'     as custom fields to [LogEvent]. If there are named arguments the names
#'     must be unique}
#'
#'   \item{`error(msg, ...)`}{Logs a message with level `error` on this logger.
#'     The arguments are interpreted as for `fatal()`.}
#'
#'   \item{`warn(msg, ...)`}{Logs a message with level `warn` on this logger.
#'     The arguments are interpreted as for `fatal()`.}
#'
#'   \item{`info(msg, ...)`}{Logs a message with level `info` on this logger.
#'     The arguments are interpreted as for `fatal()`.}
#'
#'   \item{`debug(msg, ...)`}{Logs a message with level `debug` on this logger.
#'     The arguments are interpreted as for `fatal()`.}
#'
#'   \item{`trace(msg, ...)`}{Logs a message with level `trace` on this logger.
#'   The arguments are interpreted as for `fatal()`.}
#'
#'   \item{`log(level, msg, ..., timestamp, caller)`}{
#'     If the `level` passes the Logger `threshold` a new [LogEvent]
#'     with `level`, `msg`, `timestamp` and `caller` is created. Unnamed
#'     arguments in `...` will be combined with `msg` via `base::sprintf()`.
#'     Named arguments in `...` will be passed on to `LogEvent$new()` as custom
#'     fields.
#'
#'     If the new LogEvent passes this Loggers filters, it will be dispatched
#'     to the relevant [Appenders] and checked against their thresholds and
#'     filters.
#'   }
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
#'   \item{`set_name(x)`}{Set the Logger name to the `character` scalar `x`}
#'
#'   \item{`filter(x)`}{Determine whether the LogEvent `x` should be passed
#'     on to Appenders (`TRUE`) or not (`FALSE`). See also the active binding
#'     `filters`}
#'
#'   \item{`set_filters(filters)`}{set a list of `filters`. The filters must
#'     be functions that take exactly two named arguments: the LogEvent
#'     `event` and the Logger `obj`.
#'   }
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
#'   \item{`ancestry`}{A named `logical` vector of containing
#'   the propagate value of each Logger upper the inheritance tree. The names
#'   are the names of the appenders.}
#'
#'   \item{`inherited_appenders`}{A `list` of all inherited
#'   appenders from ancestral Loggers of the current Logger}
#'
#'   \item{`filters`}{a `list` of predicates (functions that return either
#'     `TRUE` or `FALSE`). If all of these functions evaluate to `TRUE` the
#'     LogEvent is passed on to the Logger's Appenders. Since LogEvents have
#'     reference semantics, filters can also be abused to modify LogEvents
#'     before they are passed on. Look at the source code of [with_log_level()]
#'     or [with_log_value()] for examples.}
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
#' #' # the following creates a new logger that logs to a temporary file.
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
#' # use string inerpolation and custom fields
#' tf2 <- tempfile()
#' mylogger$add_appender(AppenderFile$new(tf2, layout = LayoutJson$new()))
#' mylogger$info("Not all %s support custom fields", "appenders", type = "test")
#' readLines(tf)
#' readLines(tf2)
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
      exception_handler = default_exception_handler,
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
      ...,
      timestamp = Sys.time(),
      caller = get_caller(-7)
    ){
      tryCatch({
        # preconditions
        level <- standardize_log_levels(level)
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

        # init
        msg <- as.character(msg)
        force(caller)

        if (missing(...)){
          vals <- list(
            logger = self,
            level = level,
            timestamp = timestamp,
            caller = caller,
            msg = msg
          )
        } else {
          dots <- list(...)
          if (is.null(names(dots))){
            vals <- list(
              logger = self,
              level = level,
              timestamp = timestamp,
              caller = caller,
              msg = sprintf(msg, ...)
            )
          } else {
            not_named <- vapply(names(dots), is_blank, TRUE, USE.NAMES = FALSE)
            vals <- c(list(
              logger = self,
              level = level,
              timestamp = timestamp,
              caller = caller,
              msg = do.call(sprintf, c(list(msg), dots[not_named]))
            ), dots[!not_named])
          }
        }

        # event creation needs to be as fast as possible, so we are using assign
        # to prevent the overhead of [[
        assign(
          ".last_event",
          do.call(get("new", envir = LogEvent), vals),
          private
        )

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
        msg = msg,
        caller = get_caller(-8L),
        level = 100,
        timestamp = Sys.time(),
        ...
      )
    },

    error = function(msg, ...){
      self$log(
        msg = msg,
        caller = get_caller(-8L),
        level = 200,
        timestamp = Sys.time(),
        ...
      )
    },

    warn = function(msg, ...){
      self$log(
        msg = msg,
        caller = get_caller(-8L),
        level = 300,
        timestamp = Sys.time(),
        ...
      )
    },

    info = function(msg, ...){
      self$log(
        msg = msg,
        caller = get_caller(-8L),
        level = 400,
        timestamp = Sys.time(),
        ...
      )
    },

    debug = function(msg, ...){
      self$log(
        msg = msg,
        caller = get_caller(-8L),
        level = 500,
        timestamp = Sys.time(),
        ...
      )
    },

    trace = function(msg, ...){
      self$log(
        msg = msg,
        caller = get_caller(-8L),
        level = 600,
        timestamp = Sys.time(),
        ...
      )
    },

    add_appender = function(
      appender,
      name = NULL
    ){
      assert(inherits(appender, "Appender"))
      private$.appenders[[length(private$.appenders) + 1L]] <- appender

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
      level <- standardize_threshold(level)

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
        c(setNames(self$propagate, self$name), private$.parent$ancestry),
        class = c("ancestry", "list")
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

    exception_handler = function() {private$.exception_handler},

    appenders = function() {private$.appenders},

    user = function() {private$.user}

  ),


  # private -----------------------------------------------------------------
  private = list(

    # +- methods --------------------------------------------------------------
    suspend = function(
      threshold = 0,
      inclusive = TRUE
    ){
      assert(is_scalar_bool(inclusive))
      threshold <- standardize_threshold(threshold)

      if (is.na(threshold))
        threshold <- Inf

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
        self[[nm]] <- function(...) NULL
        lockBinding(nm, env = self)
      }
      invisible(self)
    },


    unsuspend = function(
      threshold = 0,
      inclusive = TRUE
    ){
      assert(is_scalar_bool(inclusive))
      threshold <- standardize_threshold(threshold)
      if (is.na(threshold))
        threshold <- Inf

      if (inclusive){
        ll <- private$.log_levels[private$.log_levels >= threshold]
      } else {
        ll <- private$.log_levels[private$.log_levels > threshold]
      }

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

#' Demote an Exception to a Warning
#'
#' Throws a warning instead of stopping the program.
#'
#' @param e a `character` scalar (usually a `try-error`)
#'
#' @return The warning as `character` vector
#'
default_exception_handler <- function(e){
  warning(
    "[", format(Sys.time(), format = "%Y-%m-%d %H:%M:%OS3"), "] ",
    "An error occured during logging: ", e, call. = FALSE
  )
}


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
