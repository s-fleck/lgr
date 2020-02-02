loggers <- new.env()



remove_all_loggers <- function(){
  rm(list = setdiff(ls(envir = loggers), "root"), pos = loggers)
}



#' Get/Create a Logger
#'
#' @param name a `character` scalar or vector: The qualified name of the Logger
#'   as a hierarchical value.
#' @param class An [R6ClassGenerator][R6::R6] object. Usually `Logger` or `LoggerGlue`
#'   are the only valid choices.
#' @param reset a `logical` scalar. If `TRUE` the logger is reset to an
#'   unconfigured state. Unlike `$config(NULL)` this also replaces a
#'   `LoggerGlue` with vanilla `Logger`. Please note that this will invalidate
#'   Logger references created before the reset call (see examples).
#'
#' @return a [Logger]
#' @export
#'
#' @examples
#' lg <- get_logger("log/ger/test")
#' # equivalent to
#' lg <- get_logger(c("log", "ger", "test"))
#' lg$warn("a %s message", "warning")
#' lg
#' lg$parent
#'
#' if (requireNamespace('glue')){
#'   lg <- get_logger_glue("log/ger")
#' }
#' lg$warn("a {.text} message", .text = "warning")
#'
#' # completely reset 'glue' to an unconfigured vanilla Logger
#' get_logger("log/ger", reset = TRUE)
#' # WARNING: this invalidates existing references to the Logger
#' try(lg$info("lg has been invalidated an no longer works"))
#'
#' lg <- get_logger("log/ger")
#' lg$info("now all is well again")
get_logger <- function(
  name,
  class = Logger,
  reset = FALSE
){
  if (missing(name) || !length(name) || all(is_blank(name))){
    return(lgr)
  }

  assert(is.character(name))
  assert(is_scalar_bool(reset))

  nm_cur <- unlist(strsplit(name, "/", fixed = TRUE))
  name   <- paste(nm_cur, collapse = "/")
  res    <- get0(name, envir = loggers, inherits = FALSE)

  if (reset && is_Logger(res)){
    res$set_filters(function(event) stop(call. = FALSE, sprintf(paste(
      "Trying to log via a Logger reference that is no longer valid.",
      "Logger references become invalid when you reset a when you reset a",
      "Logger with `get_logger(reset = TRUE)`. Please",
      "re-create the Logger reference with with `get_logger(%s)`"), name
    )))
    res <- NULL
  }

  if (is.null(res)){
    assert(R6::is.R6Class(class), "`class` must be an R6ClassGenerator")
    assign(name, class$new(name), envir = loggers, inherits = FALSE)
    return(get_logger(name, class = class))
  }

  res
}




#' @rdname get_logger
#' @export
get_logger_glue <- function(
  name
){
  if (is_virgin_Logger(name)){
    res <- get_logger(name = name, class = LoggerGlue, reset = TRUE)
  } else {
    res <- get_logger(name = name)
  }

  assert(
    "LoggerGlue" %in% class(res),
    sprintf(
      "'%s' must be an unconfigured <Logger> or a <LoggerGlue>. You can use
      `get_logger('%s')$config(NULL)` to reset its configuration.",
      name,
      name,
      class(res)[[1]]
    )
  )

  res
}




exists_logger <- function(
  name
){
  inherits(get0(name, envir = loggers), "Logger")
}




is_virgin_Logger <- function(
  x,
  allow_subclass = FALSE
){
  assert(is_scalar_bool(allow_subclass))
  if (is.character(x)){
    x <- get_logger(x)
  } else {
    assert(is_Logger(x))
  }

  (identical(class(x), c("Logger", "Filterable", "R6")) || allow_subclass) &&
  !length(x$appenders) &&
  !length(x$filters) &&
  isTRUE(x$propagate) &&
  identical(x$exception_handler, default_exception_handler) &&
  is.null(x$.__enclos_env__$private$.threshold)
}
