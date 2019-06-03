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
#'
#' @section Usage:
#'
#' ```
#' # Cannonical way to initialize a new Logger (see "Creating Loggers")
#' lg <- get_logger("logger")
#' ```
#'
#' @eval r6_usage(Logger, name = "lg", header = "# R6 constructor (not recommended for productive use)")
#'
#' @section Creating Loggers:
#'
#' If you are a package developer you should define a new Logger for each
#' package, but you do not need to configure it. Usually only the root logger
#' needs to be configured (new Appenders added/removed, Layouts modified,
#' etc...).
#'
#' Loggers should never be instantiated directly with `Logger$new()` but rather
#' via `get_logger("name")`. If `"name"` does not exist, a new
#' Logger with that name will be created, otherwise the function returns a
#' Reference to the existing Logger.
#'
#' The `name` is potentially a `/` separated hierarchical value like
#' `foo/bar/baz`. Loggers further down the hierarchy are children of the loggers
#' above. (This mechanism does not work of the Logger is initialized with
#' `Logger$new()`)
#'
#' All calls to `get_logger()` with the same name return the same Logger
#' instance. This means that Logger instances never need to be passed between
#' different parts of an application.
#'
#' If you just want to log to an additional output (like a log file), you want
#' a new [Appender], not a new Logger.
#'
#' @inheritSection Filterable Fields
#' @section Fields:
#'
#' You can modify the fields of an existing Logger with
#' `logger$set_<fieldname>(value)` (see examples). Another way to configure
#' loggers is via its `$config()` method.
#'
#' \describe{
#'   \item{`appenders`, `set_appenders(x)`}{A single [Appender] or a `list`
#'     thereof. Appenders control the output of a Logger. Be aware that a Logger
#'     also inherits the Appenders of its ancestors
#'     (see `vignette("lgr", package = "lgr")` for more info about Logger
#'     inheritance structures).}
#'
#'   \item{`threshold`, `set_threshold(level)`}{`character` or `integer` scalar.
#'   The minimum [log level][log_levels] that triggers this Logger}
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
#'   \item{`name`}{`character` scalar. A hierarchical value
#'     (separated by `"/"``) that indicates the loggers name and its ancestors.
#'     If a logger is created with [get_logger()] uniqueness of the name is
#'     enforced.
#'   }
#'
#'   \item{`ancestry`}{A named `logical` vector of containing
#'   the propagate value of each Logger upper the inheritance tree. The names
#'   are the names of the appenders. `ancestry` is an S3 class with a custom
#'   `format()`/`print()` method, so if you want to use the plain logical
#'   vector use `unclass(lg$ancestry)`}
#'
#'   \item{`inherited_appenders`}{A `list` of all inherited
#'   appenders from ancestral Loggers of the current Logger}
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
#'   \item{`config(cfg, file, text, list`}{Load a Logger
#'     configuration. `cfg` can be either
#'   * a special list object with any or all of the the following elements:
#'     `appenders`, `threshold`, `filters`, `propagate`, `exception_handler`,
#'   * the path to a `YAML`/`JSON` config file,
#'   * a `character` scalar containing `YAML`,
#'   *  `NULL` (to reset the logger config to the default/unconfigured state)
#'
#'   The arguments `file`, `text` and `list` can be used as an alternative to
#'     `cfg` that  enforces that the  supplied  argument is of the specified
#'     type. See [logger_config] for details.
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
#'    \item{`spawn(...)`}{Spawn a child Logger.
#'      `get_logger("foo/bar")$spawn("baz")` is equivalent to
#'      `get_logger("foo/bar/baz")`, but can be convenient for programmatic use
#'      when the name of the parent Logger is not known.}
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
#' lgr$info("Today is a good day")
#' lgr$fatal("This is a serious error")
#'
#' # Loggers use sprintf() for string formatting by default
#' lgr$info("Today is %s", Sys.Date() )
#'
#' # If no unnamed `...` are present, msg is not passed through sprintf()
#' lgr$fatal("100% bad")  # so this works
#' lgr$fatal("%s%% bad", 100)  # if you use unnamed arguments, you must escape %
#'
#' # You can create new loggers with get_logger()
#' tf <- tempfile()
#' lg <- get_logger("mylogger")$set_appenders(AppenderFile$new(tf))
#'
#' # The new logger passes the log message on to the appenders of its parent
#' # logger, which is by default the root logger. This is why the following
#' # writes not only the file 'tf', but also to the console.
#' lg$fatal("blubb")
#' readLines(tf)
#'
#' # This logger's print() method depicts this relationship.
#' child <- get_logger("lg/child")
#' print(child)
#' print(child$name)
#'
#' # use formatting strings and custom fields
#' tf2 <- tempfile()
#' lg$add_appender(AppenderFile$new(tf2, layout = LayoutJson$new()))
#' lg$info("Not all %s support custom fields", "appenders", type = "test")
#' cat(readLines(tf), sep = "\n")
#' cat(readLines(tf2), sep = "\n")
#'
#' # cleanup
#' unlink(c(tf, tf2))
#' lg$config(NULL)  # reset logger config
#'
#' # LoggerGlue
#' # You can also create a new logger that uses the awesome glue library for
#' # string formatting instead of sprintf
#'
#' if (requireNamespace("glue")){
#'
#'   lg <- get_logger_glue("glue")
#'   lg$fatal("blah ", "fizz is set to: {fizz}", foo = "bar", fizz = "buzz")
#'   # prevent creation of custom fields with prefixing a dot
#'   lg$fatal("blah ", "fizz is set to: {.fizz}", foo = "bar", .fizz = "buzz")
#'
#'   #' # completely reset 'glue' to an unconfigured vanilla Logger
#'   get_logger("glue", reset = TRUE)
#'
#' }
#'
#'
#' # Configuring a Logger
#' lg <- get_logger("test")
#' lg$config(NULL)  # resets logger to unconfigured state
#'
#' # With setters
#' lg$
#'   set_threshold("error")$
#'   set_propagate(FALSE)$
#'   set_appenders(AppenderConsole$new(threshold = "info"))
#'
#' lg$config(NULL)
#'
#' # With a list
#' lg$config(list(
#'   threshold = "error",
#'   propagate = FALSE,
#'   appenders = list(AppenderConsole$new(threshold = "info"))
#' ))
#'
#' lg$config(NULL)  # resets logger to unconfigured state
#'
#' # Via YAML
#' cfg <- "
#' Logger:
#'   threshold: error
#'   propagate: false
#'   appenders:
#'     AppenderConsole:
#'       threshold: info
#' "
#'
#' lg$config(cfg)
#' lg$config(NULL)

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
      threshold = NULL,
      filters = list(),
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

      private$set_name(name)
      private$.last_event <- LogEvent$new(self)

      self$set_threshold(threshold)
      self$set_appenders(appenders)
      self$set_propagate(propagate)
      self$set_filters(filters)
      self$set_exception_handler(exception_handler)

      invisible(self)
    },


    log = function(
      level,
      msg,
      ...,
      timestamp = Sys.time(),
      caller = get_caller(-7)
    ){
      if (identical(get("threshold", envir = self), 0L))  return(invisible())

      tryCatch({
        # preconditions
        level <- standardize_log_levels(level)
        assert(
          identical(length(unique(level)), 1L),
          "Can only utilize vectorized logging if log level is the same for all entries"
        )

        # Check if LogEvent should be created
        if (
          identical(level[[1]] > get("threshold", envir = self), TRUE) ||
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
      if (isTRUE(get("threshold", envir = self) < 100L)) return(invisible())

      get("log", envir = self)(
        msg = msg,
        caller = caller,
        level = 100L,
        timestamp = Sys.time(),
        ...
      )
    },


    error = function(msg, ..., caller = get_caller(-8L)){
      if (isTRUE(get("threshold", envir = self) < 200L)) return(invisible())

      get("log", envir = self)(
        msg = msg,
        caller = caller,
        level = 200L,
        timestamp = Sys.time(),
        ...
      )
    },


    warn = function(msg, ..., caller = get_caller(-8L)){
      if (isTRUE(get("threshold", envir = self) < 300L)) return(invisible())

      get("log", envir = self)(
        msg = msg,
        caller = caller,
        level = 300L,
        timestamp = Sys.time(),
        ...
      )
    },


    info = function(msg, ..., caller = get_caller(-8L)){
      if (isTRUE(get("threshold", envir = self) < 400L))  return(invisible())

      get("log", envir = self)(
        msg = msg,
        caller = caller,
        level = 400L,
        timestamp = Sys.time(),
        ...
      )
    },


    debug = function(msg, ..., caller = get_caller(-8L)){
      if (isTRUE(get("threshold", envir = self) < 500L))  return(invisible())

      get("log", envir = self)(
        msg = msg,
        caller = caller,
        level = 500L,
        timestamp = Sys.time(),
        ...
      )
    },


    trace = function(msg, ..., caller = get_caller(-8L)){
      if (isTRUE(get("threshold", envir = self) < 600L))  return(invisible())

      get("log", envir = self)(
        msg = msg,
        caller = caller,
        level = 600L,
        timestamp = Sys.time(),
        ...
      )
    },


    config = function(
      cfg,
      file,
      text,
      list
    ){
      assert(
        missing(cfg) + missing(file) + missing(text) + missing(list) >= 3,
        "You can only specify one of `cfg`, `file`, `text` and `list`."
      )

      if (!missing(cfg)){
        if (is.list(cfg)){
          cfg <- parse_logger_config(cfg)
        } else {
          cfg <- as_logger_config(cfg)
        }

      } else if (!missing(file)){
        assert(
          is_scalar_character(file) && !grepl("\n", file) && file.exists(file),
          "`file` is not a valid path to a readable file"
        )
        cfg <- as_logger_config(file)

      } else if (!missing(text)){
        assert(
          is_scalar_character(text) && grepl("\n", text),
          "`text` must be a character scalar containing valid YAML"
        )
        cfg <- as_logger_config(text)

      } else if (!missing(list)){
        cfg <- parse_logger_config(list)

      } else {
        cfg <- as_logger_config()
      }

      if (is_logger_config(cfg)){
        cfg <- parse_logger_config(cfg)
      }

      assert(is_parsed_logger_config(cfg))
      self$set_threshold(cfg$threshold)
      self$set_appenders(cfg$appenders)
      self$set_propagate(cfg$propagate)
      self$set_filters(cfg$filters)
      self$set_exception_handler(cfg$exception_handler)

      self
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


    set_threshold = function(level){

      if (!is.null(level)){
        level <- standardize_threshold(level)
      }

      private[[".threshold"]] <- level
      invisible(self)
    },


    set_appenders = function(x){
      x <- standardize_appenders_list(x)
      private[[".appenders"]] <- list()

      for (i in seq_along(x))
        self$add_appender(x[[i]], name = names(x)[[i]])

      invisible(self)
    },


    spawn = function(name, ...){
      get_logger(paste0(self$name, "/", name))
    }
  ),

  # active bindings ---------------------------------------------------------
  active = list(
    name = function() private$.name,

    propagate = function() private$.propagate,

    last_event = function() private$.last_event,


    ancestry = function(){
      nm <- unlist(strsplit(self$name, "/"))
      res <- vapply(
        seq_along(nm),
        function(i) get_logger(nm[1:i])[["propagate"]],
        logical(1)
      )
      structure(
        setNames(res, nm),
        class = c("ancestry", class(res))
      )
    },


    parent = function() {
      if (self$name == "root"){
        return(NULL)
      } else {
        get_logger(names(self$ancestry[-length(self$ancestry)]))
      }
    },


    threshold = function() {
      res <- get(".threshold", envir = private)
      if (is.null(res)){
        get("parent", envir = self)[["threshold"]]
      } else {
        res
      }
    },


    inherited_appenders = function(){
      if (self$propagate){
        c(
          get("parent", envir = self)$appenders,
          get("parent", envir = self)$inherited_appenders
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
    set_name = function(x){
      assert(is_scalar_character(x))
      private$.name <- x
      invisible(self)
    },

    finalize = function(){
      # ensure appenders are destroyed before logger is destroyed so that the
      # finalize method of the appenders can still access the logger if it
      # wants to
      for (i in rev(seq_along(self$appenders))){
        self$remove_appender(i)
        gc()
      }

      invisible()
    },

    # +- fields ---------------------------------------------------------------
    .propagate = NULL,
    .exception_handler = NULL,
    .name = NULL,
    .appenders = NULL,
    .threshold = NULL,
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
      if (isTRUE(get("threshold", envir = self) < 100L))  return(invisible())

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
      if (isTRUE(get("threshold", envir = self) < 200L))  return(invisible())

      get("log", envir = self)(
        ...,
        caller = caller,
        level = 200L,
        timestamp = Sys.time(),
        .envir = .envir
      )
    },

    warn = function(..., caller = get_caller(-8L), .envir = parent.frame()){
      if (isTRUE(get("threshold", envir = self) < 300L))  return(invisible())

      get("log", envir = self)(
        ...,
        caller = caller,
        level = 300L,
        timestamp = Sys.time(),
        .envir = .envir
      )
    },

    info = function(..., caller = get_caller(-8L), .envir = parent.frame()){
      if (isTRUE(get("threshold", envir = self) < 400L))  return(invisible())

      get("log", envir = self)(
        ...,
        caller = caller,
        level = 400L,
        timestamp = Sys.time(),
        .envir = .envir
      )
    },

    debug = function(..., caller = get_caller(-8L), .envir = parent.frame()){
      if (isTRUE(get("threshold", envir = self) < 500L))  return(invisible())

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
      if (isTRUE(get("threshold", envir = self) < 600L))  return(invisible())

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
      if (identical(get("threshold", envir = self), 0L))  return(invisible())

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
          identical(level[[1]] > get("threshold", envir = self), TRUE) ||
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




# LoggerRoot --------------------------------------------------------------

#' Special logger subclass for the root logger. Currently exactly like a
#' normal Logger, but prevents the threshold to be set to `NULL`
#' @noRd
LoggerRoot <- R6::R6Class(
  "LoggerRoot",
  inherit = Logger,
  cloneable = FALSE,
  public <- list(
    config = function(
      cfg,
      file,
      text
    ){
      if (is.null(cfg)){
        cfg <- logger_config()
        cfg$threshold <- getOption("lgr.default_threshold", 400L)
      }

      super$config(
        cfg,
        file,
        text
      )
    },

    set_threshold = function(level){
      if (is.null(level)){
        warning("Cannot set `threshold` to `NULL` for the root Logger")
        level <- NA_integer_
      }
      level <- standardize_threshold(level)
      private[[".threshold"]] <- level
      invisible(self)
    }
  )
)




# utils -------------------------------------------------------------------

is_Logger <- function(x){
  inherits(x, "Logger")
}
