#' Loggers
#'
#' A Logger produces a [LogEvent] that contains the log message along with
#' metadata (timestamp, calling function) and dispatches it to one or several
#' [Appenders] which are responsible for the output (console, file, ...) of the
#' event. **lgr** comes with a single pre-configured Logger called the
#' `root Logger` that can be accessed via `lgr$<...>`. Instantiation of new
#' Loggers is only necessary if you want to take advantage of hierarchical
#' logging as outlined in `vignette("lgr", package = "lgr")`.
#'
#' @eval r6_usage(Logger)
#'
#' @section Creating Loggers:
#'
#' If you are a package developer you should define a new Logger for each
#' package, but you do not need to configure it. Usually only the root logger
#' needs to be configured (new Appenders added/removed, Layouts modified,
#' etc...).
#'
#' If you just want to log to an additional output (like a log file), you want
#' a new [Appender], not a new Logger.
#'
#' @inheritSection Filterable Fields
#' @section Fields:
#'
#' You can either specify these fields in `Logger$new()` or modify them after
#' creation with setter functions of the form `logger$set_<fieldname>(value)`
#' (see examples)
#'
#' \describe{
#'   \item{`name`, `set_name(x)`}{`character` scalar. A name for the Logger that should be
#'     unique among Loggers. This logger name is used in the Loggers print
#'     method and can be used by Appenders to indicate which logger the
#'     log message came from. If you define a Logger for an R Package (the most
#'     common case for defining new Loggers), the logger name should be the
#'     same name as the Package name. If you do not define a Logger name
#'     manually, a warning will be thrown.}
#'
#'   \item{`appenders`, `set_appenders(x)`}{A single [Appender] or a `list`
#'     thereof. Appenders control the output of a Logger. Be aware that a Logger
#'     also inherits the Appenders of its ancestors
#'     (see `vignette("lgr", package = "lgr")` for more info about Logger
#'     inheritance structures).}
#'
#'   \item{`threshold`, `set_threshold(level)`}{`character` or `integer` scalar.
#'   The minimum [log level][log_levels] that triggers this Logger}
#'
#'   \item{`parent`, `set_parent(logger)`}{a `Logger`. Usually the root logger.
#'   Can also be `NULL`, but all Loggers must be descendants of the root logger
#'   for lgr to work as intended.}
#'
#'   \item{`exception_handler`, `set_exception_handler()`}{a `function` that
#'   takes a single argument `e`. The function used to handle errors that occur
#'   during logging. Defaults to demoting errors to [warnings].}
#'
#'   \item{`propagate`, `set_propagate()`}{`TRUE` or `FALSE`. Should LogEvents
#'   be passed on to the appenders of the ancestral Loggers?}
#'  }
#'
#'
#' @section Read-Only Bindings:
#'
#' In addition to the active bindings used to access the fields described above,
#' Loggers also have the following additional read-only bindings:
#'
#' \describe{
#'   \item{`ancestry`}{A named `logical` vector of containing
#'   the propagate value of each Logger upper the inheritance tree. The names
#'   are the names of the appenders.}
#'
#'   \item{`inherited_appenders`}{A `list` of all inherited
#'   appenders from ancestral Loggers of the current Logger}
#'
#'   \item{`full_name`}{`character` scalar. The full or *qualified* name of
#'     the logger: The name of the logger and all loggers it inherits from
#'     (except for the root logger)
#'    }
#'
#'   \item{`last_event`}{The last LogEvent produced by the current Logger}
#' }
#'
#' @inheritSection Filterable Methods
#' @section Methods:
#'
#'
#' \describe{
#'   \item{`fatal(msg, ..., caller = get_caller(-8L))`}{Logs a message with
#'   level `fatal` on this logger. If there are *unnamed* arguments in `...`,
#'   they will be passed to `base::sprintf()` along with message. Named
#'   arguments will be passed as custom fields to [LogEvent]. If there are named
#'   arguments the names must be unique. `caller` refers to the name of the
#'   calling function and if specified manually must be a `character` scalar.}
#'
#'   \item{`error(msg, ..., caller = get_caller(-8L))`}{Logs a message with level `error` on this logger.
#'     The arguments are interpreted as for `fatal()`.}
#'
#'   \item{`warn(msg, ..., caller = get_caller(-8L))`}{Logs a message with level `warn` on this logger.
#'     The arguments are interpreted as for `fatal()`.}
#'
#'   \item{`info(msg, ..., caller = get_caller(-8L))`}{Logs a message with level `info` on this logger.
#'     The arguments are interpreted as for `fatal()`.}
#'
#'   \item{`debug(msg, ..., caller = get_caller(-8L))`}{Logs a message with level `debug` on this logger.
#'     The arguments are interpreted as for `fatal()`.}
#'
#'   \item{`trace(msg, ..., caller = get_caller(-8L))`}{Logs a message with level `trace` on this logger.
#'   The arguments are interpreted as for `fatal()`.}
#'
#'   \item{`log(level, msg, ..., timestamp, caller)`}{
#'     If the `level` passes the Logger `threshold` a new [LogEvent]
#'     with `level`, `msg`, `timestamp` and `caller` is created. Unnamed
#'     arguments in `...` will be combined with `msg` via `base::sprintf()`.
#'     Named arguments in `...` will be passed on to `LogEvent$new()` as custom
#'     fields. If no unnamed arguments are present, `msg` will *not* be passed
#'     to `sprintf()`, so in that case you do not have to escape `"%"`
#'     characters.
#'     If the new LogEvent passes this Loggers filters, it will be dispatched
#'     to the relevant [Appenders] and checked against their thresholds and
#'     filters.
#'   }
#'
#'   \item{`add_appender(appender, name = NULL)`, `remove_appender(pos)`}{
#'     Add or remove an [Appender]. Supplying a `name` is optional but
#'     recommended. After adding an Appender with
#'     `logger$add_appender(AppenderConsole$new(), name = "console")` you can
#'      refer to it via `logger$appenders$console`. `remove_appender()` can
#'      remove an Appender by position or name.
#'    }
#'
#'    \item{`spawn(...)`}{Spawn a child Logger. `lg <- lgr$spawn("mylogger")` is
#'      equivalent to `lg <- Logger$new("mylogger", parent = lgr)`}
#' }
#'
#'
#' @section LoggerGlue:
#'
#' `LoggerGlue` uses [glue::glue()] instead of [base::sprintf()] to construct
#' log messages. **glue** is a very well designed package for
#' string interpolation. It makes composing log messages
#' more flexible and comfortable at the price of an additional dependency and
#' slightly less performance than `sprintf()`.
#'
#' `glue()` lets you define temporary named variables inside the call.
#' As with the normal Logger, these named arguments get turned into custom
#' fields; however, you can suppress this behaviour by making named argument
#' start with a `"."`. Please refer to `vignette("lgr", package = "lgr")` for
#' examples.
#'
#' @name Logger
#' @aliases Loggers LoggerGlue
#' @include Filterable.R
#' @include log_levels.R
#' @seealso [glue](https://glue.tidyverse.org/)
#' @examples
#' # lgr::lgr is the root logger that is always available
#' lgr$info("Today is %s", Sys.Date() )
#' lgr$fatal("This is a serious error")
#'
#' # You can create new loggers with Logger$new(). The following creates a
#' # logger that logs to a temporary file.
#' tf <- tempfile()
#' lg <- Logger$new("mylogger", appenders = AppenderFile$new(tf))
#'
#' # The new logger passes the log message on to the appenders of its parent
#' # logger, which is by default the root logger. This is why the following
#' # writes not only the file 'tf', but also to the console.
#' lg$fatal("blubb")
#' readLines(tf)
#'
#' # This logger's print() method depicts this relationship
#' lg2 <- Logger$new("child", parent = lg)
#' print(lg2)
#' print(lg2$ancestry)
#' print(lg2$full_name)
#'
#' # use formatting strings and custom fields
#' tf2 <- tempfile()
#' lg$add_appender(AppenderFile$new(tf2, layout = LayoutJson$new()))
#' lg$info("Not all %s support custom fields", "appenders", type = "test")
#' cat(readLines(tf), sep = "\n")
#' cat(readLines(tf2), sep = "\n")
#'
#' # The following works because if no unnamed `...` are present, msg is not
#' # passed through sprintf() (otherwise you would have to escape the "%")
#' lg$fatal("100%")
#'
#' # LoggerGlue
#' lg <- LoggerGlue$new("glue")
#' lg$fatal("blah ", "fizz is set to: {fizz}", foo = "bar", fizz = "buzz")
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
      name = "(unnamed logger)",
      appenders = list(),
      threshold = 400L,
      filters = list(),
      parent = lgr::lgr,
      exception_handler = default_exception_handler,
      propagate = TRUE
    ){
      # fields
      # threshold must be set *after* the logging functions have been initalized
      if (identical(name, "(unnamed logger)")){
        warning(
          "When creating a new Logger, you should assign it a unique `name`. ",
          "Please see ?Logger for more infos.", call. = FALSE
        )
      }

      self$set_appenders(appenders)
      self$set_exception_handler(exception_handler)
      self$set_parent(parent)
      self$set_name(name)
      self$set_propagate(propagate)
      self$set_filters(filters)
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
      if (identical(get(".threshold", envir = private), 0L)) return(NULL)

      tryCatch({
        # preconditions
        level <- standardize_log_levels(level)
        assert(
          identical(length(unique(level)), 1L),
          "Can only utilize vectorized logging if log level is the same for all entries"
        )

        # Check if LogEvent should be created
        if (
          identical(level[[1]] > get(".threshold", envir = private), TRUE) ||
          identical(getOption("lgr.logging_suspended"), TRUE)
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
            msg  <- sprintf(msg, ...)
            vals <- list(
              logger = self,
              level = level,
              timestamp = timestamp,
              caller = caller,
              msg = msg
            )
          } else {
            not_named <- vapply(names(dots), is_blank, TRUE, USE.NAMES = FALSE)
            if (any(not_named)){
              msg <- do.call(sprintf, c(list(msg), dots[not_named]))
            }

            vals <- c(
              list(
                logger = self,
                level = level,
                timestamp = timestamp,
                caller = caller,
                msg = msg
              ),
                dots[!not_named]
              )
          }
        }

        # This code looks really weird, but it really is just replacing all
        # instances of [[ with get() for minimal overhead. We want event
        # dispatch to be as quick as possible.
        event <- do.call(get("new", envir = LogEvent), vals)
        assign(".last_event", event, private)

        if (get("filter", envir = self)(event)){
          for (app in unlist(mget(c("appenders", "inherited_appenders"), self), recursive = FALSE)){
            app_thresh <- get("threshold", envir = app)
            if (
              (is.na(app_thresh) || get("level", envir = event) <= app_thresh) &&
              get("filter", envir = app)(event)
            ){
              get("append", envir = app)(event)
            }
          }
        }

        invisible(msg)
      },
        error = get("handle_exception", envir = self)
      )
    },


    fatal = function(msg, ..., caller = get_caller(-8L)){
      if (isTRUE(get(".threshold", envir = private) < 100L)) return(NULL)

      get("log", envir = self)(
        msg = msg,
        caller = caller,
        level = 100L,
        timestamp = Sys.time(),
        ...
      )
    },

    error = function(msg, ..., caller = get_caller(-8L)){
      if (isTRUE(get(".threshold", envir = private) < 200L)) return(NULL)

      get("log", envir = self)(
        msg = msg,
        caller = caller,
        level = 200L,
        timestamp = Sys.time(),
        ...
      )
    },

    warn = function(msg, ..., caller = get_caller(-8L)){
      if (isTRUE(get(".threshold", envir = private) < 300L)) return(NULL)

      get("log", envir = self)(
        msg = msg,
        caller = caller,
        level = 300L,
        timestamp = Sys.time(),
        ...
      )
    },

    info = function(msg, ..., caller = get_caller(-8L)){
      if (isTRUE(get(".threshold", envir = private) < 400L)) return(NULL)

      get("log", envir = self)(
        msg = msg,
        caller = caller,
        level = 400L,
        timestamp = Sys.time(),
        ...
      )
    },

    debug = function(msg, ..., caller = get_caller(-8L)){
      if (isTRUE(get(".threshold", envir = private) < 500L)) return(NULL)

      get("log", envir = self)(
        msg = msg,
        caller = caller,
        level = 500L,
        timestamp = Sys.time(),
        ...
      )
    },

    trace = function(msg, ..., caller = get_caller(-8L)){
      if (isTRUE(get(".threshold", envir = private) < 600L)) return(NULL)

      get("log", envir = self)(
        msg = msg,
        caller = caller,
        level = 600L,
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

      private$.appenders <- list()

      for (i in seq_along(x))
        self$add_appender(x[[i]], name = names(x)[[i]])

      invisible(self)
    },


    spawn = function(...){
      Logger$new(..., parent = self)
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

    full_name = function(){
      paste(rev(setdiff(names(self$ancestry), "root")), collapse = ".")
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

    appenders = function() {private$.appenders}

  ),


  # private -----------------------------------------------------------------
  private = list(

    # +- fields ---------------------------------------------------------------
    .propagate = NULL,
    .exception_handler = NULL,
    .name = NULL,
    .parent = NULL,
    .appenders = NULL,
    .threshold = NA_integer_,
    .last_event = NULL
  )
)




# LoggerGlue --------------------------------------------------------------

#' @export
LoggerGlue <- R6::R6Class(
  "LoggerGlue",
  inherit = Logger,
  cloneable = FALSE,

  public = list(

    fatal = function(..., caller = get_caller(-8L), .envir = parent.frame()){
      if (isTRUE(get(".threshold", envir = private) < 100L)) return(NULL)

      force(.envir)
      get("log", envir = self)(
        ...,
        caller = caller,
        level = 100L,
        timestamp = Sys.time(),
        .envir = .envir
      )
    },

    error = function(..., caller = get_caller(-8L), .envir = parent.frame()){
      if (isTRUE(get(".threshold", envir = private) < 200L)) return(NULL)

      get("log", envir = self)(
        ...,
        caller = caller,
        level = 200L,
        timestamp = Sys.time(),
        .envir = .envir
      )
    },

    warn = function(..., caller = get_caller(-8L), .envir = parent.frame()){
      if (isTRUE(get(".threshold", envir = private) < 300L)) return(NULL)

      get("log", envir = self)(
        ...,
        caller = caller,
        level = 300L,
        timestamp = Sys.time(),
        .envir = .envir
      )
    },

    info = function(..., caller = get_caller(-8L), .envir = parent.frame()){
      if (isTRUE(get(".threshold", envir = private) < 400L)) return(NULL)

      get("log", envir = self)(
        ...,
        caller = caller,
        level = 400L,
        timestamp = Sys.time(),
        .envir = .envir
      )
    },

    debug = function(..., caller = get_caller(-8L), .envir = parent.frame()){
      if (isTRUE(get(".threshold", envir = private) < 500L)) return(NULL)

      force(.envir)
      get("log", envir = self)(
        ...,
        caller = caller,
        level = 500L,
        timestamp = Sys.time(),
        .envir = .envir
      )
    },

    trace = function(..., caller = get_caller(-8L), .envir = parent.frame()){
      if (isTRUE(get(".threshold", envir = private) < 600L)) return(NULL)

      force(.envir)
      get("log", envir = self)(
        ...,
        caller = caller,
        level = 600L,
        timestamp = Sys.time(),
        .envir = .envir
      )
    },

    log = function(
      level,
      ...,
      timestamp = Sys.time(),
      caller = get_caller(-7),
      .envir = parent.frame()
    ){
      if (identical(get(".threshold", envir = private), 0L)) return(NULL)

      force(.envir)
      tryCatch({
        # preconditions
        level <- standardize_log_levels(level)
        assert(
          identical(length(unique(level)), 1L),
          "Can only utilize vectorized logging if log level is the same for all entries"
        )

        # Check if LogEvent should be created
        if (
          identical(level[[1]] > get(".threshold", envir = private), TRUE) ||
          identical(getOption("lgr.logging_suspended"), TRUE)
        ){
          return(invisible(msg))
        }

        # init
        # the do.call is so that `...` gets evaluated in the correct environment
        msg <- glue::glue( ..., .envir = .envir)
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
          custom_fields <- !(grepl("^\\.", names(dots)) | is_blank(names(dots)))

          vals <- c(
            list(
              logger = self,
              level = level,
              timestamp = timestamp,
              caller = caller,
              msg = msg
            ),
            dots[custom_fields]
          )
        }

        # This code looks really weird, but it really is just replacing all
        # instances of [[ with get() for minimal overhead. We want event
        # dispatch to be as quick as possible.
        event <- do.call(get("new", envir = LogEvent), vals)
        assign(".last_event", event, private)

        if (get("filter", envir = self)(event)){
          for (app in unlist(mget(c("appenders", "inherited_appenders"), self), recursive = FALSE)){
            app_thresh <- get("threshold", envir = app)
            if (
              (is.na(app_thresh) || get("level", envir = event) <= app_thresh) &&
              get("filter", envir = app)(event)
            ){
              get("append", envir = app)(event)
            }
          }
        }

        invisible(msg)
      },
      error = get("handle_exception", envir = self)
      )
    }
  )
)

