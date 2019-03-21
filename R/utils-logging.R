#' Suspend All Logging
#'
#' Completely disable logging for all loggers. This is for example useful for
#' automated test code. `suspend_logging()` globally disables all logging with
#' lgr until `unsuspend_logging()` is invoked, while `without_logging()` and
#' `with_logging()` temporarily disable/enable logging.
#'
#' @return
#'   `suspend_logging()` and `unsuspend_logging()` return `NULL` (invisibly),
#'   `without_logging()` and `with_logging()` returns whatever `code` returns.
#' @export
#' @examples
#' lg <- get_logger("test")
#'
#' # temporarily disable logging
#' lg$fatal("foo")
#' without_logging({
#'   lg$info("everything in this codeblock will be suppressed")
#'   lg$fatal("bar")
#' })
#'
#' # globally disable logging
#' suspend_logging()
#' lg$fatal("bar")
#' with_logging(lg$fatal("foo"))  # log anyways
#'
#' # globally enable logging again
#' unsuspend_logging()
#' lg$fatal("foo")
suspend_logging <- function(){
  options("lgr.logging_suspended" = TRUE)
  invisible()
}




#' @rdname suspend_logging
#' @export
unsuspend_logging <- function(){
  options("lgr.logging_suspended" = FALSE)
  invisible()
}




#' @rdname suspend_logging
#' @param code Any \R code
#' @export
without_logging <- function(code){
  old <- getOption("lgr.logging_suspended")
  on.exit(options(lgr.logging_suspended = old))
  suspend_logging()
  force(code)
}





#' @rdname suspend_logging
#' @export
with_logging <- function(code){
  old <- getOption("lgr.logging_suspended")
  on.exit(options(lgr.logging_suspended = old))
  unsuspend_logging()
  force(code)
}





#' Inject Values into Logging Calls
#'
#' `with_log_level` temporarily overrides the log level of all [LogEvents]
#' created by target [Logger].
#'
#' These functions abuses lgr's filter mechanic to modify LogEvents in-place
#' before they passed on the Appenders. Use with care as they can
#' produce hard to reason about code.
#'
#' @param level `integer` or `character` scalar: the desired log level
#' @param code Any \R code
#' @param logger a [Logger]. defaults to the root logger (lgr::lgr).
#'
#' @return whatever `code` would return
#' @export
#' @examples
#' with_log_level("warn", {
#'   lgr$info("More important than it seems")
#'   FATAL("Really not so bad")
#' })
with_log_level <- function(
  level,
  code,
  logger = lgr::lgr
){
  level <- standardize_log_level(level)
  force(level)

  set_level <- function(event){
    event[["level"]]  <- level
    TRUE
  }

  # to make it unlikely something goes wrong if people do funky stuff with
  # filters inside with_log_level calls
  tn <- paste0("...WITH_LOG_LEVEL_TEMP_FILTER", sample.int(1e9, size = 1))
  logger$add_filter(set_level, name = tn)
  on.exit(logger$remove_filter(tn))

  force(code)
}




#' `with_log_value()` injects arbitrary values into all [LogEvents] (overriding
#' existing ones). This is especially powerful in combination with Appenders
#' that support arbitrary log fields, like [AppenderJson].
#'
#' @param values a named `list` of values to be injected into the logging calls
#' @rdname with_log_level
#' @export
#' @examples
#'
#' with_log_value(
#'   list(msg = "overriden msg"),  {
#'   lgr$info("bar")
#'   INFO("FOO")
#' })
with_log_value <- function(
  values,
  code,
  logger = lgr::lgr
){
  assert(is_equal_length(names(values), values))

  set_level <- function(event){
    for (i in seq_along(values)){
      event[[names(values)[[i]] ]] <- values[[i]]
    }
    TRUE
  }

  tn <- paste0("...WITH_LOG_VALUE_TEMP_FILTER", sample.int(1e9, size = 1))
  logger$add_filter(set_level, name = tn)
  on.exit(logger$remove_filter(tn))

  force(code)
}
