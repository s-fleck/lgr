#' Simple Logging
#'
#' @name simple_logging
#'
#' @examples
#' FATAL("This is an important message about %s going wrong", "something")
#' DEBUG("Debug messages are hidden by default")
#' threshold("debug")  # you must use lower case names here
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
#'   yog provides some functions for managing loggers that default to the root
#'   logger as target. These are useful if you just use the root logger and
#'   do not want to worry about
#'
#'   `threshold()` sets or retrieves the threshold for an appender or logger
#'   (the minimum level of log messages it processes).
#'
#'   `add_appender()` and `remove_appender()` add [Appenders] to [Loggers] and
#'   other Appenders.
#'
#' @rdname simple_logging
#' @return
#'   `threshold()` returns the log level of `target` as `integer` (invisibly)
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
#' @param appender an `Appender`
#' @return `add_appender()` and `remove_appender()` return `target`
#' @export
add_appender <- function(
  appender,
  target = yog::yog
){
  target$add_appender(appender)
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
  if (inherits(target, "Appender"))
    return(target$show(n = n, threshold = threshold))

  sel <- vapply(target$appenders, inherits, TRUE, "AppenderMemoryDt")

  if (!any(sel)){
    warning(sprintf(
      "This %s has no memory appender (see ?AppenderMemoryDt)",
      class_fmt(target)
    ))
    return(invisible())

  } else {
    target$appenders[sel][[1]]$show(n = n, threshold = threshold)
  }
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
