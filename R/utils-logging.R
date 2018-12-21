

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




#' Override Log Level
#'
#' Temporarily override the log level of all [LogEvents] created by target
#' [Logger]
#'
#' This abuses yog's [filter] mechanic to modify LogEvents in-place before it
#' is passed on the appenders.
#'
#' @param level `integer` or `character` scalar: the desired log level
#' @param code Any \R code
#' @param logger a [Logger]. defaults to the root logger (yog::yog).
#' @export
#' @examples
#' with_log_level("warn", {
#'   yog$info("More important than it seems")
#'   FATAL("Really not so bad")
#' })
with_log_level <- function(
  level,
  code,
  logger = yog::yog
){
  assert_valid_log_levels(level)
  if (is.character(level)){
    level <- unlabel_levels(level)
  }

  force(level)
  set_level <- function(event, obj){
    event[["level"]] <- level
    TRUE
  }

  filters_orig <- logger$filters
  on.exit(logger$set_filters(c(filters_orig)))

  logger$set_filters(c(set_level, filters_orig))

  force(code)
}
