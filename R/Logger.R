#' Loggers
#'
#' A Logger produces a [LogEvent] that contains a log message along with
#' metadata (timestamp, calling function, ...) and dispatches it to one or
#' more [Appenders] which are responsible for the output (console, file, ...)
#' of the event. **lgr** comes with a single pre-configured Logger called the
#' `root Logger` that can be accessed via `lgr$<...>`. Instantiation of new
#' Loggers is done with [get_logger()]. It is advisable to instantiate a
#' separate Logger with a descriptive name for each package/script in which
#' you use \pkg{lgr}.
#'
#' @name Logger
#' @aliases Loggers
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

    #' @description
    #'
    #' **Loggers should never be instantiated directly with `Logger$new()`** but
    #' rather via [`get_logger("name")`][get_logger]. This way new Loggers are
    #' registered in a global namespace which ensures uniqueness and
    #' facilitates inheritance between Loggers. If `"name"` does not exist, a
    #' new Logger with that name will be created, otherwise the function returns
    #' a Reference to the existing Logger.
    #'
    #' `name` is potentially a `"/"` separated hierarchical value like
    #' `foo/bar/baz`. Loggers further down the hierarchy are descendants of the
    #' loggers above and (by default) inherit `threshold` and `Appenders` from
    #' their ancestors.
    #'
    #' @note
    #' If you are a package developer you should define a new Logger for each
    #' package, but you do not need to configure it. The user of the package
    #' should decide how and where to output logging, usually by configuring the
    #' root Logger (new Appenders added/removed, Layouts modified, etc...).
    #'
    #' @param name,appenders,threshold,filters,exception_handler,propagate
    #'   See section Active bindings.
    #'
    #' @seealso [get_logger()]
    #'
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


    #' @description Log an event.
    #'
    #' If `level` passes the Logger's `threshold` a new [LogEvent] with `level`,
    #' `msg`, `timestamp` and `caller` is created.  If the new LogEvent also
    #' passes the Loggers [Filters][EventFilter], it is be dispatched to the
    #' relevant [Appenders].
    #'
    #' @param level a `character` or `integer` scalar. See [log_levels].
    #'
    #' @param msg `character`. A log message. If unnamed arguments are supplied
    #' in `...`, `msg` is passed on to [base::sprintf()] (which means `"%"` have
    #' to be escaped), otherwise `msg` is left as-is.
    #'
    #' @param ... *unnamed* arguments in `...` must be `character` scalars and
    #' are passed to [base::sprintf()]. *Named* arguments must have unique names
    #' but can be arbitrary \R objects that are passed to [`LogEvent$new()`][LogEvent] and
    #' will be turned into custom fields.
    #'
    #' @param timestamp [POSIXct]. Timestamp of the event.
    #' @param caller a `character` scalar. The name of the calling function.
    log = function(
      level,
      msg,
      ...,
      timestamp = Sys.time(),
      caller = get_caller(-7)
    ){
      if (identical(get("threshold", envir = self), 0L))  return(invisible())
      LOG_CALL <- sys.call(-1L)  # for error reporting

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
        if (inherits(msg, "condition")){
          msg <- conditionMessage(msg)
        } else {
          msg <- as.character(msg)
        }

        force(caller)
        rawMsg <- msg

        if (missing(...)){
          vals <- list(
            logger = self,
            level = level,
            timestamp = timestamp,
            caller = caller,
            msg = msg,
            .rawMsg = rawMsg
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
              msg = msg,
              .rawMsg = rawMsg
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
                msg = msg,
                .rawMsg = rawMsg
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
            tryCatch({
              app_thresh <- get("threshold", envir = app)
              if (
                (is.na(app_thresh) || get("level", envir = event) <= app_thresh) &&
                get("filter", envir = app)(event)
              ){
                get("append", envir = app)(event)
              }
            }, error = function(e) {
              e$call <- LOG_CALL
              e$appender <- app
              e$logger <- self
              get("handle_exception", envir = self)(e)
            }
          )
        }
      }

        invisible(msg)
      },
        error = function(e) {
          e$logger <- self
          e$call <- LOG_CALL
          get("handle_exception", envir = self)(e)
        }
      )
    },

    #' @description Log an Event fatal priority
    #' @param msg,...,caller see `$log()`
    fatal = function(msg, ..., caller = get_caller(-8L)){
      if (identical(get("threshold", envir = self) < 100L, TRUE))
        return(invisible())

      get("log", envir = self)(
        msg = msg,
        caller = caller,
        level = 100L,
        timestamp = Sys.time(),
        ...
      )
    },


    #' @description Log an Event error priority
    #' @param msg,...,caller see `$log()`
    error = function(msg, ..., caller = get_caller(-8L)){
      if (identical(get("threshold", envir = self) < 200L, TRUE))
        return(invisible())

      get("log", envir = self)(
        msg = msg,
        caller = caller,
        level = 200L,
        timestamp = Sys.time(),
        ...
      )
    },


    #' @description Log an Event warn priority
    #' @param msg,...,caller see `$log()`
    warn = function(msg, ..., caller = get_caller(-8L)){
      if (identical(get("threshold", envir = self) < 300L, TRUE))
        return(invisible())

      get("log", envir = self)(
        msg = msg,
        caller = caller,
        level = 300L,
        timestamp = Sys.time(),
        ...
      )
    },


    #' @description Log an Event info priority
    #' @param msg,...,caller see `$log()`
    info = function(msg, ..., caller = get_caller(-8L)){
      if (identical(get("threshold", envir = self) < 400L, TRUE))
        return(invisible())

      get("log", envir = self)(
        msg = msg,
        caller = caller,
        level = 400L,
        timestamp = Sys.time(),
        ...
      )
    },


    #' @description Log an Event debug priority
    #' @param msg,...,caller see `$log()`
    debug = function(msg, ..., caller = get_caller(-8L)){
      if (identical(get("threshold", envir = self) < 500L, TRUE))
        return(invisible())

      get("log", envir = self)(
        msg = msg,
        caller = caller,
        level = 500L,
        timestamp = Sys.time(),
        ...
      )
    },


    #' @description Log an Event trace priority
    #' @param msg,...,caller see `$log()`
    trace = function(msg, ..., caller = get_caller(-8L)){
      if (identical(get("threshold", envir = self) < 600L, TRUE))
        return(invisible())

      get("log", envir = self)(
        msg = msg,
        caller = caller,
        level = 600L,
        timestamp = Sys.time(),
        ...
      )
    },


    #' @description
    #' `list_log()` is a shortcut for `do.call(Logger$log, x)`.
    #' See \url{https://github.com/s-fleck/joblog} for an R package that
    #' leverages this feature to create custom log event types for tracking
    #' the status of cron jobs.
    #'
    #' @param x a named `list` that must at least contain the named elements
    #'   `level` and `timestamp`
    #'
    #' @examples
    #' lg <- get_logger("test")
    #' lg$list_log(list(level = 400, msg = "example"))
    list_log = function(x){
      do.call(self$log, x)
    },



    #' @description Load a Logger configuration.
    #'
    #' @param cfg
    #' * a special `list` object with any or all of the the following elements:
    #'     `appenders`, `threshold`, `filters`, `propagate`, `exception_handler`,
    #'
    #' * the path to a `YAML`/`JSON` config file,
    #'
    #' * a `character` scalar containing `YAML/JSON`,
    #'
    #' * `NULL` (to reset the logger config to the default/unconfigured state)
    #'
    #' @param file,text,list can be used as an alternative to
    #'   `cfg` that  enforces that the  supplied  argument is of the specified
    #'   type. See [logger_config] for details.
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


    #' @description Add an Appender to the Logger
    #'
    #' @param appender a single [Appender]
    #' @param name a `character` scalar. Optional but recommended.
    #'
    #' @examples
    #' lg <- get_logger("test")
    #' lg$add_appender(AppenderConsole$new(), name = "myconsole")
    #' lg$appenders[[1]]
    #' lg$appenders$myconsole
    #' lg$remove_appender("myconsole")
    #' lg$config(NULL)  # reset config
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


    #' @param pos `integer` index or `character` name of the Appender(s) to
    #' remove
    #'
    #' @description remove an appender
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


    #' @description
    #' To prevent errors in the logging logic from crashing the whole script,
    #' Loggers pass errors they encounter to an exception handler. The default
    #' behaviour is to demote errors to [warnings]. See also
    #' `set_exception_handler()`.
    #'
    #' @param expr expression to be evaluated.
    handle_exception = function(expr){
      private$.exception_handler(expr)
    },


    #' @description Set the exception handler of a logger
    #'
    #' @param fun a `function` with the single argument `e` (an error [condition])
    #'
    #' @examples
    #' lgr$info(stop("this produces a warning instead of an error"))
    set_exception_handler = function(fun){
      assert(is.function(fun))
      private$.exception_handler <- fun
      invisible(self)
    },


    #' @description Should a Logger propagate events to the Appenders of its ancestors?
    #' @param x `TRUE` or `FALSE`. Should [LogEvents] be passed on to the appenders
    #' of the ancestral Loggers?
    set_propagate = function(x){
      assert(is_scalar_bool(x))
      private$.propagate <- x
      invisible(self)
    },


    #' @description Set the minimum log level of events that a Logger should process
    #'
    #' @param level `character` or `integer` scalar. The minimum
    #' [log level][log_levels] that triggers this Logger
    set_threshold = function(level){
      if (!is.null(level))
        level <- standardize_threshold(level)

      private[[".threshold"]] <- level
      invisible(self)
    },


    #' @description Set the Logger's Appenders
    #'
    #' @param x single [Appender] or a `list` thereof. Appenders control the
    #'   output of a Logger. Be aware that a Logger also inherits the Appenders
    #'   of its ancestors (see `vignette("lgr", package = "lgr")` for more info
    #'   about Logger inheritance).
    set_appenders = function(x){
      x <- standardize_appenders_list(x)
      private[[".appenders"]] <- list()

      for (i in seq_along(x))
        self$add_appender(x[[i]], name = names(x)[[i]])

      invisible(self)
    },


    #' @description Spawn a child Logger.
    #' This is very similar to using [get_logger()], but
    #' can be useful in some cases where Loggers are created programmatically
    #'
    #' @param name `character` vector. Name of the child logger
    #'  `get_logger("foo/bar")$spawn("baz")` is equivalent
    #'   to `get_logger("foo/bar/baz")`
    spawn = function(name){
      get_logger(c(private[[".name"]], name))
    }
  ),


  # active bindings ---------------------------------------------------------
  active = list(

    #' @field name A `character` scalar. The unique name of each logger,
    #' which also includes the names of its ancestors (separated by `/`).
    name = function(){
      paste(get(".name", envir = private), collapse = "/")
    },


    #' @field threshold `integer` scalar. The threshold of the `Logger`, or if it
    #' `NULL` the threshold it inherits from its closest ancestor with a
    #' non-`NULL` threshold
    threshold = function() {
      res <- get(".threshold", envir = private)
      if (is.null(res)){
        get("threshold", envir = get("parent", envir = self))
      } else {
        res
      }
    },


    #' @field propagate A `TRUE` or `FALSE`. The unique name of each logger,
    #' which also includes the names of its ancestors (separated by `/`).
    propagate = function(){
      get(".propagate", envir = private)
    },


    #' @field ancestry A named `logical` vector of containing the propagate value
    #'   of each Logger upper the inheritance tree. The names are the names of
    #'   the appenders. `ancestry` is an S3 class with a custom
    #'   `format()`/`print()` method, so if you want to use the plain logical
    #'   vector use `unclass(lg$ancestry)`
    ancestry = function(){
      nm  <- get(".name", envir = private)
      res <- logical(length(nm))

      for (i in seq_along(nm)){
        res[[i]] <- get("propagate", envir = get_logger(nm[1:i]))
      }

      structure(
        setNames(res, nm),
        class = c("ancestry", class(res))
      )
    },


    #' @field parent a `Logger`. The direct ancestor of the `Logger`.
    parent = function() {
      nm <- get(".name", envir = private)

      if (identical(nm, "root")){
        return(NULL)
      } else {
        get_logger(nm[-length(nm)])
      }
    },


    #' @field last_event The last LogEvent produced by the current Logger
    last_event = function() {
      get(".last_event", envir = private)
    },


    #' @field appenders a `list` of all [Appenders] of the Logger
    appenders = function() {
      get(".appenders", envir = private)
    },


    #' @field inherited_appenders A `list` of all appenders that the Logger
    #'   inherits from its ancestors
    inherited_appenders = function(){
      if (get(".propagate", envir = private)){
        p <- get("parent", envir = self)
        if (is.null(p))
          return(NULL)

        unlist(
          unname(mget(c("appenders", "inherited_appenders"), envir = p)),
          recursive = FALSE
        )
      } else {
        NULL
      }
    },


    #' @field exception_handler a `function`. See `$set_exception_handler` and
    #'   `$handle_exception`
    exception_handler = function() {
      get(".exception_handler", envir = private)
    }
  ),


  # private -----------------------------------------------------------------
  private = list(
    set_name = function(x){
      assert(is_scalar_character(x))
      private$.name <- unlist(strsplit(x, "/"))
      invisible(self)
    },

    #' @description
    #' Ensure all Appenders attached to a Logger are destroyed before the
    #' Logger, so that the Appender's finalize method can access the logger if
    #' it wants to
    finalize = function(){

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

#' LoggerGlue
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
#' @export
LoggerGlue <- R6::R6Class(
  "LoggerGlue",
  inherit = Logger,
  cloneable = FALSE,

  public = list(

    fatal = function(..., caller = get_caller(-8L), .envir = parent.frame()){
      if (identical(get("threshold", envir = self) < 100L, TRUE))
        return(invisible())

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
      if (identical(get("threshold", envir = self) < 200L, TRUE))
        return(invisible())

      get("log", envir = self)(
        ...,
        caller = caller,
        level = 200L,
        timestamp = Sys.time(),
        .envir = .envir
      )
    },

    warn = function(..., caller = get_caller(-8L), .envir = parent.frame()){
      if (identical(get("threshold", envir = self) < 300L, TRUE))
        return(invisible())

      get("log", envir = self)(
        ...,
        caller = caller,
        level = 300L,
        timestamp = Sys.time(),
        .envir = .envir
      )
    },

    info = function(..., caller = get_caller(-8L), .envir = parent.frame()){
      if (identical(get("threshold", envir = self) < 400L, TRUE))
        return(invisible())

      get("log", envir = self)(
        ...,
        caller = caller,
        level = 400L,
        timestamp = Sys.time(),
        .envir = .envir
      )
    },

    debug = function(..., caller = get_caller(-8L), .envir = parent.frame()){
      if (identical(get("threshold", envir = self) < 500L, TRUE))
        return(invisible())

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
      if (identical(get("threshold", envir = self) < 600L, TRUE))
        return(invisible())

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
      if (identical(get("threshold", envir = self), 0L))
        return(invisible())

      LOG_CALL <- sys.call(-1L)  # for error reporting

      force(.envir)
      tryCatch({
        # preconditions
        level <- standardize_log_levels(level)
        assert(
          identical(length(unique(level)), 1L),
          "Can only utilize vectorized logging if log level is the same for all entries"
        )

        assert(
          !missing(...),
          "No log message or structured logging fields supplied"
        )

        dots <- list(...)

        if ("msg" %in% names(dots)){
          warning(
            "LoggerGlue does not support a `$msg` argument. Please use unnamed ",
            "text strings for construction your message like in `glue::glue()`.",
            call. = FALSE
          )
          names(dots)[names(dots) == "msg"] <- ""
        }

        # construct msg
        dots_msg <- dots  # because we need the original dots later again

        sel <- names(dots_msg) == ""
        if (!length(sel)){
          sel <- seq_along(dots_msg)
        }

        dots_msg[sel] <- lapply(dots_msg[sel], function(e){
          if (inherits(e, "condition")){
            conditionMessage(e)
          } else {
            as.character(e)
          }
        })

        rawMsg <- dots[[1]]
        msg <- do.call(glue::glue, args = c(dots_msg, list(.envir = .envir)))

        # Check if LogEvent should be created
        if (
          identical(level[[1]] > get("threshold", envir = self), TRUE) ||
          identical(getOption("lgr.logging_suspended"), TRUE)
        ){
          return(invisible(msg))
        }

        force(caller)

        # Create list that contains all values equired for the log event
        custom_fields <- !(grepl("^\\.", names(dots)) | is_blank(names(dots)))

        vals <- c(
          list(
            logger = self,
            level = level,
            timestamp = timestamp,
            caller = caller,
            msg = msg,
            .rawMsg = rawMsg
          ),
          dots[custom_fields]
        )


        # This code looks really weird, but it really is just replacing all
        # instances of [[ with get() for minimal overhead. We want event
        # dispatch to be as quick as possible.
        event <- do.call(get("new", envir = LogEvent), vals)
        assign(".last_event", event, private)

        if (get("filter", envir = self)(event)){
          for (app in unlist(mget(c("appenders", "inherited_appenders"), self), recursive = FALSE)){
            tryCatch({
              app_thresh <- get("threshold", envir = app)
              if (
                (is.na(app_thresh) || get("level", envir = event) <= app_thresh) &&
                get("filter", envir = app)(event)
              ){
                get("append", envir = app)(event)
              }
            }, error = function(e) {
              e$call <- LOG_CALL
              e$appender <- app
              e$logger <- self
              get("handle_exception", envir = self)(e)
            }
          )
        }
      }
        invisible(msg)
      },
        error = function(e){
          e$call <- LOG_CALL
          e$logger <- self
          get("handle_exception", envir = self)(e)
        }
      )
    },


    list_log = function(x){
       # TODO: Workaround until we have a better implementation of list_log
      names(x)[names(x) == "msg"] <- ""
      do.call(self$log, x)
    },


    spawn = function(name){
      get_logger_glue(c(private[[".name"]], name))
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




# print.Logger ------------------------------------------------------------

#' Print a Logger Object
#'
#' The `print()` method for Loggers displays the most important aspects of
#' the Logger.
#'
#'
#' @param x any \R Object
#' @param color `TRUE` or `FALSE`: Output with color? Requires the Package
#'   **crayon**
#' @param ... ignored
#'
#' @return
#'   `print()` returns `x` (invisibly), `format()` returns a `character` vector.
#' @export
#'
#' @examples
#' # print most important details of logger
#' print(lgr)

print.Logger <- function(
  x,
  color = requireNamespace("crayon", quietly = TRUE),
  ...
){
  assert(is_scalar_bool(color))
  cat(format(x, color = color), sep = "\n")
  invisible(x)
}




#' @export
#' @rdname print.Logger
format.Logger = function(
  x,
  color = FALSE,
  ...
){
  assert(is_scalar_bool(color))
  assert(is_scalar_bool(color))
  if (!color) style_subtle <- identity

  thr <- fmt_threshold(x$threshold, type = "character")

  if (is_threshold_inherited(x)){
    thr <- style_subtle(thr)
  }


  header <- paste(
    paste0("<", class(x)[[1]], "> [", thr, "]"),
    format(x$ancestry, color = color)
  )

  appenders <- appender_summary(x$appenders)
  inherited_appenders  <- appender_summary(x$inherited_appenders)

  ind <- "  "
  res <- header

  if (!is.null(appenders)){
    res <-c(
      res, "",
      "appenders:", paste0(ind, appenders)
    )
  }

  if (!is.null(inherited_appenders)){
    res <- c(
      res, "",
      "inherited appenders:", paste0(ind, inherited_appenders)
    )
  }

  res
}



appender_summary <- function(x){
  dd <- lapply(x, srs_appender)

  if (!length(x)){
    return(NULL)
  }

  if (is.null(names(x))){
    names(x) <- ""
  }


  dd <- do.call(rbind, dd)

  if (is.null(dd)) return(NULL)

  dd$name <- ifelse(
    is.na(names(x)) | is_blank(names(x)),
    paste0("[[", seq_len(length(x)), "]]"),
    names(x)
  )

  dd$destination <- ifelse(
    !is_blank(dd$destination),
    paste("->", dd$destination),
    ""
  )

  with(
    dd,
    paste0(
      pad_right(name), ": ",
      pad_right(class), " [",
      pad_left(fmt_threshold(threshold, type = "character")), "] ",
      destination
    )
  )
}



# single-row-summary
srs_appender <- function(x){
  data.frame(
    class = fmt_class(class(x)[[1]]),
    threshold = x$threshold,
    destination = x$destination
  )
}




#' @description
#' You can also print just the `ancestry` of a Logger which can be accessed with
#' with `logger$ancestry()`. This returns a named `character` vector whose
#' names correspond to the names of the Loggers `logger` inherits from. The
#' `TRUE`/`FALSE` status of its elements correspond to the `propagate` values of
#' these Loggers.
#'
#' @rdname print.Logger
#' @export
#'
#' @examples
#' # print only the ancestry of a logger
#' lg <- get_logger("AegonV/Aerys/Rheagar/Aegon")
#' get_logger("AegonV/Aerys/Rheagar")$set_propagate(FALSE)
#'
#' print(lg$ancestry)
#' unclass(lg$ancestry)

print.ancestry <- function(
  x,
  color = requireNamespace("crayon", quietly = TRUE),
  ...
){
  assert(is_scalar_bool(color))
  cat(format(x, color = color), "\n")
  invisible(x)
}




#' @export
#' @rdname print.Logger
format.ancestry <- function(
  x,
  color = FALSE,
  ...
){
  assert(is_scalar_bool(color))
  sps <- rep("/", length(x))
  nms <- names(x)

  style <- identity

  if (color){
    for (i in rev(seq_along(x))){
      nms[[i]] <- style(nms[[i]])
      sps[[i]] <- style(sps[[i]])
      if (!x[[i]]){
        style <- style_subtle
      }
    }
  }

  sps[length(sps)] <- ""

  paste0(nms, sps, collapse = "")
}




is_threshold_inherited <- function(x){
  is.null(x[[".__enclos_env__"]][["private"]][[".threshold"]])
}

