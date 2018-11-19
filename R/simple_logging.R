#' Simple Logging
#'
#' @param msg,... passed on to [base::sprintf()]
#' @param level the log level, either as integer or character (see bellow)
#' @param threshold like `log_level`, but used to limit print output
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
#' @param level
#' @param target a [Logger] or [Appender]. Defaults to the root logger.
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
    target$threshold <- level

  invisible(target$threshold)
}



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
  target$remove_appender(appender)
}





show = function(
  n = 20,
  threshold = NA,
  logger = yog::yog
){
  sel <- vapply(self$appenders, inherits, TRUE, "AppenderMemoryDt")

  if (!any(sel)){
    message("This logger has no memory appender (see ?AppenderMemoryDt)")
    return(invisible())

  } else {
    self$appenders[sel][[1]]$show(n = n, threshold = threshold)
  }
}
