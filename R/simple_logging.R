#' Simple Logging
#'
#' These functions provide a simple interface to the root logger. If you do not
#' need any of the more advanced features of lgr, start here.
#'
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



#' @export
#' @rdname simple_logging
log_exception <- function(
  ...
){
  tryCatch(
    ...,
    error = function(e){
      FATAL(unlist(strsplit(e$message, split = "\n", fixed = TRUE)))
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




#' @param n `integer` scalar. Show only the last `n` log entries that match
#'   `threshold`
#' @rdname simple_logging
#' @export
show_log = function(
  threshold = NA_integer_,
  n = 20L,
  target = lgr::lgr
){
  assert(
    is_scalar_integerish(n),
    "'n' must be an integer scalar, not a ", class_fmt(n)
  )
  threshold <- standardize_threshold(threshold)

  if (inherits(target, "Appender")){
    if ("show" %in% names(target)){
      return(target$show(n = n, threshold = threshold))
    } else {
      stop(
        "This ", class_fmt(target, c("R6", "Filterable", "Appender")),
        " does not have a `show()` method"
      )
    }

  } else if (inherits(target, "Logger")){
    sel <- vapply(target$appenders, function(app) "show" %in% names(app), logical(1))
    if (!any(sel)){
      stop(sprintf(
        "This %s has no Appender with a `show()` method (see ?AppenderTabular)",
        class_fmt(target, c("R6", "Filterable", "Appender"))
      ))
      return(invisible())
    }
    return(target$appenders[sel][[1]]$show(n = n, threshold = threshold))
  }

  stop(sprintf("'%s' is not a valid `target` for show_log()", class_fmt(target)))
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
