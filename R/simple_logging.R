#' Simple Logging
#'
#' These functions provide a simple interface to the root logger. If you do not
#' need any of the more advanced features of lgr, start here.
#'
#' @name simple_logging
#'
#' @examples
#' FATAL("This is an important message about %s going wrong", "something")
#' DEBUG("Debug messages are hidden by default")
#' console_threshold("debug")  # you must use lower case names here
#' DEBUG("Unless we lower the threshold")
#'
NULL




#' Basic Setup for the Logging System
#'
#' Quick and easy way to configure the root logger for logging to a file.
#' **experimental**, paramters will likely change in the next lgr version.
#'
#' @param file `character` scalar: If not `NULL` a [AppenderFile] will be created
#'   that logs to this file. If the filename ends in `.jsonl` an [AppenderJson]
#'   will be created instead.
#' @param fmt `character` scalar: Format to use if `file` is supplied and not
#'   a `.jsonl` file. If `NULL` it defaults to `"%L [%t] %m"`
#'   (see [format.LogEvent])
#' @inheritParams print.LogEvent
#' @inheritParams Logger
#' @param appenders a single [Appender] or a list thereof. Must be `NULL` if
#'   if `file` is already specified.
#' @param threshold `character` or `integer` scalar.
#'   The minimum [log level][log_levels] that should be processed by the root
#'   logger.
#' @param memory `logical` scalar. add a memory appender
#' @param console `logical` scalar. add a console appender
#'
#' @return the `root` Logger (lgr)
#' @export
#'
#' @examples
#' \dontrun{
#' make the root logger log to a file
#' basic_config(file = tempfile())
#' }
basic_config <- function(
  file = NULL,
  fmt = NULL,
  timestamp_fmt = "%Y-%m-%d %H:%M:%OS3",
  threshold = "info",
  console = TRUE,
  memory = TRUE,
  appenders = NULL
){
  warning("This function is still experimental and paramters might change in the near future", call. = FALSE)

  l <- get_logger("root")
  l$config(logger_config(threshold = NA_integer_))  # reset root logger config


  # threshold
  assert(!is.null(threshold))
  l$set_threshold(threshold)


  if (!is.null(file)){
    assert(is.null(appenders), "`appenders` must be NULL if `file` is specified")
    pos <- regexpr("\\.([[:alnum:]]+)$", file)
    ext <- ifelse(pos > -1L, substring(file, pos + 1L), "")

    if (identical(tolower(ext), "jsonl")){
      assert (is.null(fmt), "`fmt` must be null if `file` is a '.jsonl' file")
      l$add_appender(
        name = "file",
        AppenderJson$new(threshold = NA)
      )

    } else {
      if (is.null(fmt))  fmt <- "%L [%t] %m"

      l$add_appender(
        name = "file",
        AppenderFile$new(
          file = file,
          threshold = NA,
          layout = LayoutFormat$new(
            fmt = fmt,
            timestamp_fmt = timestamp_fmt
          )
        )
      )
    }
  }

  if (console){
    if (is.null(fmt))  fmt <- "%L [%t] %m"
    l$add_appender(
      name = "console",
      AppenderConsole$new(
        threshold = NA,
        layout = LayoutFormat$new(
          colors = getOption("lgr.colors"),
          fmt = fmt,
          timestamp_fmt = timestamp_fmt
        )
      )
    )
  }

  if (memory){
    l$add_appender(name = "memory", AppenderBuffer$new(
      threshold = NA,
      should_flush = function(event) FALSE
    ))
  }


  lgr
}





# logging -----------------------------------------------------------------

#' @param msg,... passed on to [base::sprintf()]
#' @param level,threshold an `integer` or `character` scalar, see
#'   `getOption("lgr.log_levels")` for possible values. For `threshold` `0`
#'   (`"off"`) and `NA` (`"all"`) are also valid.
#' @param target a [Logger] or [Appender]. Defaults to the root logger.
#'
#' @export
#' @return `FATAL()` ... `TRACE()` and `log_exception()` return the log message
#'   as a `character` vector.
#' @rdname simple_logging
FATAL <- function(msg, ...){
  lgr$fatal(msg, ...)
}




#' @export
#' @rdname simple_logging
ERROR <- function(msg, ...){
  lgr$error(msg, ...)
}




#' @export
#' @rdname simple_logging
WARN <- function(msg, ...){
  lgr$warn(msg, ...)
}




#' @export
#' @rdname simple_logging
INFO <- function(msg, ...){
  lgr$info(msg, ...)
}




#' @export
#' @rdname simple_logging
DEBUG <- function(msg, ...){
  lgr$debug(msg, ...)
}




#' @export
#' @rdname simple_logging
TRACE <- function(msg, ...){
  lgr$trace(msg, ...)
}




#' @inheritParams with_log_value
#' @param logfun a `function` for processing the log request, usually
#'   `lgr$info()`, `lgr$debug()`, etc... .
#' @param caller a `character` scalar. The name of the calling function
#'
#' @export
#' @rdname simple_logging
log_exception <- function(
  code,
  logfun = lgr$fatal,
  caller = get_caller(-3)
){
  force(caller)
  force(logfun)
  tryCatch(
    force(code),
    error = function(e){
      logfun(unlist(strsplit(e$message, split = "\n", fixed = TRUE)), caller = caller)
      stop(e)
    }
  )
}




# managment ---------------------------------------------------------------

#' @description
#'   lgr provides convenience functions to manage the root Logger. These
#'   are intended for interactive use, and for people who just need basic
#'   logging facilities and don't want to worry about hierarchical loggers and
#'   R6 classes.
#'
#'   `threshold()` sets or retrieves the threshold for an [Appender] or [Logger]
#'   (the minimum level of log messages it processes). It's `target` defaults
#'   to the root logger.
#'
#'   `console_threshold()` is a shortcut to set the threshold of the root
#'   loggers [AppenderConsole], which is usually the only Appender that manages
#'   console output for a given \R session.
#'
#'   `add_appender()` and `remove_appender()` add Appenders to Loggers and
#'   other Appenders.
#'
#' @rdname simple_logging
#' @return
#'   `threshold()` and `console_threshold()` return the [log_level] of `target`
#'   as `integer` (invisibly)
#'
#' @export
threshold <- function(
  level,
  target = lgr::lgr
){
  if (missing(level))
    target$threshold
  else
    target$set_threshold(level)

  invisible(target$threshold)
}




#' @rdname simple_logging
#' @export
console_threshold <- function(
  level,
  target = lgr::lgr$appenders$console
){
  assert(inherits(target, "AppenderConsole"))
  if (missing(level))
    target$threshold
  else
    target$set_threshold(level)

  invisible(target$threshold)
}




#' @rdname simple_logging
#' @param appender an `Appender`
#' @param name `character` scalar. An optional name for the new Appender.
#' @return `add_appender()` and `remove_appender()` return `target`.
#' @export
#' @examples
#' add_appender(AppenderConsole$new(), "second_console_appender")
#' FATAL("Multiple console appenders are a bad idea")
#' remove_appender("second_console_appender")
#' INFO("Good that we defined an appender name, so it's easy to remove")
add_appender <- function(
  appender,
  name = NULL,
  target = lgr::lgr
){
  target$add_appender(appender, name = name)
}




#' @param pos `integer` index or `character` names of the appenders to remove
#' @rdname simple_logging
#' @export
remove_appender <- function(
  pos,
  target = lgr::lgr
){
  target$remove_appender(pos)
}




#' @description
#' `show_log()` displays the last `n` log entries of `target` if `target` is
#' an Appender with a `show()` method or a Logger with at least one such
#' Appender attached. `target` defaults to the root logger, which has an
#' [AppenderDt] attached by default if the package **data.table** is installed.
#' With the default logging settings this includes also `TRACE` and `DEBUG`
#' messages, even if they were not printed to the console before.
#'
#' `show_data()` and `show_dt()` work similar to `show_log()`, except that
#' they return the log as `data.frame` or `data.table` respectively.
#'
#' @param n `integer` scalar. Show only the last `n` log entries that match
#'   `threshold`
#'
#' @return `show_log()` prints to the console and returns whatever the target
#'   Appender's `$show()` method returns, usually a `data.frame` or `data.table`
#'   (invisibly).
#'
#'   `show_data()` always returns a `data.frame` and `show_dt()` always returns
#'   a `data.table`.
#' @rdname simple_logging
#' @export
show_log <- function(
  threshold = NA_integer_,
  n = 20L,
  target = lgr::lgr
){
  dd <- find_target(target, "show")
  dd$show(n = n, threshold = threshold)
}




#' @rdname simple_logging
#' @export
show_dt <- function(
  target = lgr::lgr
){
  dd <- try(find_target(target, "dt"), silent = TRUE)

  if (inherits(dd, "try-error")){
    dd <- find_target(target, "data")
    data.table::as.data.table(dd$data)
  } else {
    dd$dt
  }
}




#' @rdname simple_logging
#' @export
show_data <- function(
  target = lgr::lgr
){
  dd <- find_target(target, "dt")
  as.data.frame(dd$data)
}




#' Return the first Appender that has a method or field called `name`
#'
#' @param x a [Logger] or [Appender]
#' @param name
#'
#' @return an Appender
#' @noRd
find_target <- function(
  x,
  name
){
  assert(is_scalar_character(name))

  if (inherits(x, "Appender")){
    if (name %in% names(x)){
      return(x)
    } else {
      stop(
        "This ", class_fmt(x, c("R6", "Filterable", "Appender")),
        " has no method or field called `", name, "`"
      )
    }

  } else if (inherits(x, "Logger")){
    for (app in x$appenders){
      res <- try(find_target(app, name), silent = TRUE)
      if (inherits(res, "Appender")) return(res)
    }
    stop(
      "This ", class_fmt(x, c("R6", "Filterable")),
      " has no Appender with a field or method called `", name, "`"
    )
  } else {
    stop("`x` is not a Logger or Appender")
  }
}




# simple setup ------------------------------------------------------------

default_appenders <- function(
){
  log_files  <- getOption("lgr.log_file", NULL)
  thresholds <- names(log_files)

  if (is.null(thresholds)){
    thresholds <- rep(NA, length(log_files))
  }

  lf <- lapply(seq_along(log_files), function(i){
    tryCatch(
      setup_file_appender(log_files[[i]], thresholds[[i]]),
      error = function(e){
        warning(
          sprintf(paste(
            "Cannot setup logfile '%s' as specified in the global options:",
            "%s\nSee '?lgr' for help."),
            log_files[[i]], e)
        )
        NULL
      }
    )
  })
}




setup_file_appender <- function(path, threshold = NA){

  if (grepl("\\.json.*", path)){
    lo <- LayoutJson$new()
  } else {
    lo <- LayoutFormat$new()
  }

  AppenderFile$new(file = path, threshold = threshold, layout = lo)
}
