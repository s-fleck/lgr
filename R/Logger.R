#' Loggers
#'
#' * A Logger records the log message and some metadata (timestamp,
#'   calling function) as a [LogEvent]
#' * Several [Appenders] can be attached to Loggers to write events to a
#'   destination, for example the console or a text file (a Logger without
#'   Appenders does nothing useful).
#' * A [Layout] is used by the Appender to convert the LogEvent to the
#'   appropriate Format for output.
#' * **Log levels** reflect the importance of a log event. Loggers and
#'   Appenders have a **thresholds**, that is the minimum log level that will
#'   be processed by said Logger/Appender.
#'
#'
#' @section Usage:
#'
#' ```
#' l <- Logger$new("example logger")
#'
#' # methods
#'  l$fatal(msg, ...)
#'  l$error(msg, ...)
#'  l$warn(msg, ...)
#'  l$info(msg, ...)
#'  l$debug(msg, ...)
#'  l$trace(msg, ...)
#'  l$log(level, msg, timestamp = Sys.time(), caller = get_caller())
#'  l$add_appender(appender, name = NULL)
#'  l$remove_appender(pos)
#'  l$handle_exception(e)
#'  l$filter(event)
#'
#' # fields / active bindings
#'  l$appenders
#'  l$ancestry
#'  l$ancestral_appenders
#'  l$filters
#'  l$last_event
#'  l$name
#'  l$parent
#'  l$propagate
#'  l$threshold
#'  l$user
#'
#' ```
#'
#' @section Creating a new Logger:
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
#'   \item{`handle_exception(e)`}{Used to handle errors that occur durring the
#'   logging process. The defaul is to convert errors to warnings.}
#'
#'   \item{`filter(x)`}{Determine whether the LogEvent `x` should be passed
#'   on to Appenders (`TRUE`) or not (`FALSE`). See also the active binding
#'   `filters`}
#' }
#'
#'
#' @section Active Bindings:
#'
#' \describe{
#'
#'   \item{`appenders`}{A `list` of all Appenders attached to the current
#'     logger}
#'
#'   \item{`ancestry`}{*read only*. A `character` vector of the names of all
#'     Loggers that are ancestors to the current Logger}
#'
#'   \item{`ancestral_appenders`}{*read only*. A `list` of all inherited
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
#'   \item{`propagate`}{`TRUE` or `FALSE`. Should log messages be passed on to
#'   the appenders of the ancestral Loggers?}
#'
#'   \item{`threshold`}{An `integer`. Threshold of the current Logger (i.e the
#'   maximum log level that this Logger processes)}
#'
#'   \item{`user`}{The current user}
#' }
#'
#' @name Logger
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
      handle_exception = function(e){
        warning(
          "[", format(Sys.time(), format = "%Y-%m-%d %H:%M:%OS3"), "] ",
          "An error occured in the logging sub system: ", e
        )
      },
      propagate = TRUE
    ){
      # fields
      # threshold must be set *after* the logging functions have been initalized
      self$appenders <- appenders
      self$user  <- user
      self$handle_exception <- handle_exception
      self$parent <- parent
      self$name <- name
      self$last_event <- LogEvent$new(self)
      self$propagate <- propagate


      # init log functions
      make_logger <- function(
        level,
        ...
      ){
        force(level)
        function(msg, ...){
          self$log(
            msg = private$.string_formatter(msg, ...),
            caller = get_caller(-4L),
            level = level
          )
        }
      }

      for (i in seq_along(private$.log_levels)){
        nm  <- names(private$.log_levels)[[i]]
        lvl <- private$.log_levels[[i]]
        if (nm %in% names(self)){
          stop(
            "The following names are not allowed for log levels: ",
            paste(sort(names(self)), collapse= ", ")
          )
        }

        self[[nm]] <- make_logger(lvl)
      }

      self$threshold <- threshold
      invisible(self)
    },


    finalize = function(){
      # ensure appenders are destroyed before logger is destroyed so that the
      # finalize method of the appenders can still access the logger if it
      # wants to

      for (i in rev(seq_along(self$appenders))){
        self$remove_appender(i)
      }

      gc()
    },


    log = function(
      level,
      msg,
      timestamp = Sys.time(),
      caller = get_caller(-3)
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

        # check threshold
        thr <- private$.threshold
        if (is.na(thr)) thr <- Inf
        if (level[[1]] > thr) return(invisible())

        # update log event
        assign("level", level, envir = self$last_event)
        assign("timestamp", timestamp,  envir = self$last_event)
        assign("caller", caller, envir = self$last_event)
        assign("msg", msg, envir = self$last_event)

        # emit
        if (self$filter(self$last_event)){
          for (app in c(self$appenders, self$ancestral_appenders)) {
            if (app$filter(self$last_event)){
              app$append(self$last_event)
            }
          }
        }
      },
        error = self$handle_exception
      )
    },


    handle_exception = NULL,


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
      appender$logger <- self


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

    # +- fields -----------------------------------------------------------
    last_event = NULL
  ),

  # active bindings ---------------------------------------------------------
  active = list(
    name = function(value){
      if (missing(value)) return(private$.name)
      assert(is_scalar_character(value))
      private$.name <- value
    },


    propagate = function(value){
      if (missing(value)) return(private$.propagate)
      assert(is_scalar_bool(value))
      private$.propagate <- value
    },


    ancestry = function(){
      structure(
        c(self$name, private$.parent$ancestry),
        class = c("ancestry", "character")
      )
    },


    parent = function(value){
      if (missing(value)) return(private$.parent)
      assert(is.null(value) || inherits(value, "Logger"))
      private$.parent <- value
    },



    threshold = function(value){
      if (missing(value))  return(private$.threshold)

      assert_valid_threshold(value)
      if (is_scalar_character(value))  value <- unlabel_levels(value)

      private$unsuspend()
      private$suspend(value, inclusive = FALSE)

      private$.threshold <- as.integer(value)
    },


    ancestral_appenders = function(){
      if (self$propagate){
        c(
          private$.parent$appenders,
          private$.parent$ancestral_appenders
        )
      } else {
        NULL
      }
    },


    appenders = function(value){
      if (missing(value)) return(c(private$.appenders))

      if (is.null(value)){
        private$.appenders <- list()
        return(invisible())
      }

      if (inherits(value, "Appender"))
        value <- list(value)

      assert(
        is.list(value) && all(vapply(value, inherits, TRUE, "Appender")),
        "'appenders' must either be a single Appender, a list thereof, or ",
        "NULL for no appenders."
      )

      for (i in seq_along(value))
        self$add_appender(value[[i]], name = names(value)[[i]])

      invisible()
    },


    user = function(value){
      if (missing(value)) return(private$.user)
      assert(
        is_scalar_character(value),
        "'user' must be a scalar character"
      )
      private$.user <- value
    }
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
        self[[nm]] <- function(...) invisible()
      }
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
        self[[nm]] <- private$suspended_loggers[[nm]]
      }
      private$suspended_loggers <- list()
    },


    # +- fields ---------------------------------------------------------------
    .propagate = NULL,
    .filters = list(check_threshold),
    .name = NULL,
    .parrent = NULL,
    .appenders = NULL,
    .user = NA_character_,
    .threshold = 4L,
    .string_formatter = sprintf,

    # intentionaly hardcoded and not using the global options. this is used to
    # track which logging functions are available for the logger
    .log_levels = as_log_levels(c(
      "fatal" = 100L,
      "error" = 200L,
      "warn"  = 300L,
      "info"  = 400L,
      "debug" = 500L,
      "trace" = 600L
    )),
    suspended_loggers = list()
  ),

  lock_objects = FALSE
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



object_summaries <- function(x, exclude = NULL) {
  if (length(x) == 0)
    return(NULL)

  if (is.list(x))
    obj_names <- names(x)
  else if (is.environment(x))
    obj_names <- ls(x, all.names = TRUE)

  obj_names <- setdiff(obj_names, exclude)

  values <- vapply(obj_names, function(name) {
    if (is.environment(x) && bindingIsActive(name, x)) {
      "active binding"
    } else {
      obj <- .subset2(x, name)
      if (is.function(obj)) deparse(args(obj))[[1L]]
      # Plain environments (not envs with classes, like R6 or RefClass objects)
      else if (is.environment(obj) && identical(class(obj), "environment")) "environment"
      else if (is.null(obj)) "NULL"
      else if (is.atomic(obj)) {
        # If obj has many elements, paste() can be very slow, so we'll just
        # use just a subset of it. https://github.com/r-lib/R6/issues/159
        txt <- as.character(utils::head(obj, 60))
        txt <- paste(txt, collapse = " ")
        trim(txt)
      }
      else paste(class(obj), collapse = ", ")
    }
  }, FUN.VALUE = character(1))

  paste0(obj_names, ": ", values, sep = "")
}
