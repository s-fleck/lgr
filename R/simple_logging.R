#' Simple Logging
#'
#' lgr provides convenience functions managing the root Logger. These are
#' designed chiefly for interactive use and are less verbose than their
#' R6 method counterparts.
#'
#' @name simple_logging
#'
NULL





# logging -----------------------------------------------------------------

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

#' @param target a [Logger] or [Appender] or the name of a Logger as `character`
#'   scalar
#'
#' @description
#'   `threshold()` sets or retrieves the threshold for an [Appender] or [Logger]
#'   (the minimum level of log messages it processes). It's `target` defaults to
#'   the root logger. (equivalent to `lgr::lgr$threshold` and
#'   `lgr::lgr$set_threshold`)
#'
#'   `console_threshold()` is a shortcut to set the threshold of the root
#'   loggers [AppenderConsole], which is usually the only Appender that manages
#'   console output for a given \R session. (equivalent to
#'   `lgr::lgr$appenders$console$threshold` and
#'   `lgr::lgr$appenders$console$set_threshold`)
#'
#'   `add_appender()` and `remove_appender()` add Appenders to Loggers and other
#'   Appenders. (equivalent to `lgr::lgr$add_appender` and
#'   `lgr::lgr$remove_appender`)
#'
#' @rdname simple_logging
#' @return
#'   `threshold()` and `console_threshold()` return the [log_level] of `target`
#'   as `integer` (invisibly)
#'
#' @export
#'
#'
#' @examples
#' # Get and set the threshold of the root logger
#' threshold("error")
#' threshold()
#' lgr$info("this will be supressed")
#' lgr$error("an important error message")
#'
#' # you can also specify a target to modify other loggers
#' lg <- get_logger("test")
#' threshold("fatal", target = lg)
#' threshold(target = lg)
#'
#' # If a Logger's threshold is not set, the threshold is inherited from
#' # its parent, in this case the root logger (that we set to error/200 before)
#' threshold(NULL, target = lg)
#' threshold(target = lg)
#'
#' # Alternative R6 API for getting/setting thresholds
#' lg$set_threshold("info")
#' lg$threshold
#' lg$set_threshold(300)
#' lg$threshold
#' lg$set_threshold(NULL)
#' lg$threshold
#'
#' # cleanup
#' lgr$config(NULL)
#' lg$config(NULL)
threshold <- function(
  level,
  target = lgr::lgr
){
  if (missing(level))
    return(target[["threshold"]])
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
#'
#'
#' # add Appenders to a Logger
#' add_appender(AppenderConsole$new(), "second_console_appender")
#' lgr$fatal("Multiple console appenders are a bad idea")
#' remove_appender("second_console_appender")
#' lgr$info("Good that we defined an appender name, so it's easy to remove")
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
#' `show_log()` displays the last `n` log entries of an Appender (or a Logger
#' with such an Appender attached) with a `$show()` method. Most, but not all
#' Appenders support this function (try [AppenderFile] or [AppenderBuffer]).
#'
#' `show_data()` and `show_dt()` work similar to `show_log()`, except that
#' they return the log as `data.frame` or `data.table` respectively. Only
#' Appenders that log to formats that can easily be converted to `data.frames`
#' are supported (try [AppenderJson] or [AppenderBuffer]).
#'
#' The easiest way to try out this features is by adding an AppenderBuffer
#' to the root logger with [`basic_config(memory = TRUE)`][basic_config()].
#'
#' @param n `integer` scalar. Show only the last `n` log entries that match
#'   `threshold`
#'
#' @inheritParams basic_config
#'
#' @return `show_log()` prints to the console and returns whatever the target
#'   Appender's `$show()` method returns, usually a `character` vector,
#'   `data.frame` or `data.table` (invisibly).
#'
#'   `show_data()` always returns a `data.frame` and `show_dt()` always returns
#'   a `data.table`.
#' @rdname simple_logging
#' @export
#'
#' @examples
#'
#' # Reconfigure the root logger
#' basic_config(memory = TRUE)
#'
#' # log some messages
#' lgr$info("a log message")
#' lgr$info("another message with data", data = 1:3)
#'
#' show_log()
#' show_data()
#'
#' # cleanup
#' lgr$config(NULL)
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
  tryCatch(
    find_target(target, "dt")[["dt"]],
    error = function(e){
      data.table::as.data.table(find_target(target, "data")[["data"]])
    }
  )
}




#' @rdname simple_logging
#' @export
show_data <- function(
  target = lgr::lgr
){
  dd <- find_target(target, "data")
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

  if (is_scalar_character(x)) {
    x <- get_logger(x)
  }

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
