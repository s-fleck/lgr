

#' Suspend All Logging
#'
#' Completely disable logging for all loggers. This is for example useful for
#' automated test code.
#'
#' @return
#'   `suspend_logging()` and `unsuspend_logging()` return `NULL` (invisibly),
#'   `without_logging` returns whatever `code` returns
#' @export
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

  # fix the caller
  caller <- get_caller(-2L)
  force(caller)

  set_level <- function(event, obj){
    event[["level"]]  <- level
    event[["caller"]] <- caller
    TRUE
  }

  filters_orig <- logger$filters
  on.exit(logger$set_filters(c(filters_orig)))
  logger$set_filters(c(set_level, filters_orig))

  force(code)
}



#' `with_log_value()` injects arbitrary values into all [LogEvents] (overriding
#' existing ones). This is especially powerfull in combination with Appenders
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

  # fix the caller
  if (!"caller" %in% names(values)){
    values[["caller"]] <- get_caller(-2L)
  }

  set_level <- function(event, obj){
    for (i in seq_along(values)){
      event[[names(values)[[i]] ]] <- values[[i]]
    }
    TRUE
  }

  filters_orig <- logger$filters
  on.exit(logger$set_filters(c(filters_orig)))
  logger$set_filters(c(set_level, filters_orig))

  force(code)
}
