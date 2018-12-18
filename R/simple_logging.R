#' Simple Logging
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
#'   `getOption("yog.log_levels")` for possible values. For `threshold` `0`
#'   (`"off"`) and `NA` (`"all"`) are also valid.
#' @param target a [Logger] or [Appender]. Defaults to the root logger.
#'
#' @export
#' @return `FATAL()` ... `TRACE()` and `log_exception()` return the log message
#'   as a `character` vector.
#' @rdname simple_logging
FATAL <- function(msg, ...){
  yog$fatal(msg, ...)
}




#' @export
#' @rdname simple_logging
ERROR <- function(msg, ...){
  yog$error(msg, ...)
}




#' @export
#' @rdname simple_logging
WARN <- function(msg, ...){
  yog$warn(msg, ...)
}




#' @export
#' @rdname simple_logging
INFO <- function(msg, ...){
  yog$info(msg, ...)
}




#' @export
#' @rdname simple_logging
DEBUG <- function(msg, ...){
  yog$debug(msg, ...)
}




#' @export
#' @rdname simple_logging
TRACE <- function(msg, ...){
  yog$trace(msg, ...)
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
#'   Yog provides convenience functions to manage the root Logger. These
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
  target = yog::yog
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
  target = yog::yog$appenders$console
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
  target = yog::yog
){
  target$add_appender(appender, name = name)
}



#' @param pos `integer` index or `character` names of the appenders to remove
#' @rdname simple_logging
#' @export
remove_appender <- function(
  pos,
  target = yog::yog
){
  target$remove_appender(pos)
}




#' @param n `integer` scalar. Show only the last `n` log entries that match
#'   `threshold`
#' @rdname simple_logging
#' @export
show_log = function(
  n = 20,
  threshold = NA,
  target = yog::yog
){
  if (!requireNamespace("data.table")){
    stop(
      "To use this feature, please install the package data.table via ",
      "install.packages('data.table') and restart R."
    )
  }

  if (inherits(target, "Appender")){
    if ("show" %in% names(target))
      return(target$show(n = n, threshold = threshold))
    else
      stop("This ", class_fmt(target), " does not have a 'show()' method")
  }


  sel <- vapply(target$appenders, function(app) "show" %in% names(app), logical(1))

  if (!any(sel)){
    warning(sprintf(
      "This %s has no appender with a show method (see ?AppenderTabular)",
      class_fmt(target)
    ))
    return(invisible())
  }

  target$appenders[sel][[1]]$show(n = n, threshold = threshold)
}




#' Suspend all logging
#'
#' Completely disable logging for all loggers. This is for example useful for
#' automated test code.
#'
#' @return
#'   `suspend_logging()` and `unsuspend_logging()` return `NULL` (invisibly),
#'   `without_logging` returns whatever `code` returns
#' @export
suspend_logging <- function(){
  options("yog.logging_suspended" = TRUE)
  invisible()
}




#' @rdname suspend_logging
#' @export
unsuspend_logging <- function(){
  options("yog.logging_suspended" = FALSE)
  invisible()
}




#' @rdname suspend_logging
#' @param code Any \R code
#' @export
#' @examples
#' without_logging({
#'   FATAL("FOO")
#'   INFO("BAR")
#' })
#'
without_logging <- function(code){
  suspend_logging()
  on.exit(unsuspend_logging())
  force(code)
}




# simple setup ------------------------------------------------------------

default_appenders <- function()
{
  log_files  <- getOption("yog.log_file", NULL)
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
            "%s\nSee '?yog' for help."),
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
