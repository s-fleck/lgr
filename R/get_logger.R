loggers <- new.env()




#' Get/Create a Logger
#'
#' @param name a `character` scalar or vector: The qualified name of the Logger
#'   as a hierarchical value.
#' @param class An [R6ClassGenerator][R6::R6] object. Usually `Logger` or `LoggerGlue`
#'   are the only valid choices.
#' @param reset a `logical` scalar. If `TRUE` the logger is reset to an
#'   unconfigured state. Unlike `$config(NULL)` this also replaces a
#'   `LoggerGlue` with vanilla `Logger`.
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
#' # completely reset 'glue' to an unconfigured vanilla Logger
#' get_logger("glue", reset = TRUE)
get_logger <- function(
  name,
  class = Logger,
  reset = FALSE
){
  if (missing(name) || !length(name) || all(is_blank(name))){
    return(lgr)
  }
  nm_cur <- unlist(strsplit(name, "/", fixed = TRUE))
  name   <- paste(nm_cur, collapse = "/")
  res    <- get0(name, envir = loggers)

  if (inherits(res, class$classname) || reset){
    return(res)
  }

  assign(
    name,
    class$new(name),
    envir = loggers
  )

  get_logger(name)
}




#' @rdname get_logger
#' @export
get_logger_glue <- function(
  name
){
  if (is_virgin_Logger(name)){
    res <- get_logger(name = name, class = LoggerGlue)
  } else {
    res <- get_logger(name = name)
  }

  assert(
    inherits(res, "LoggerGlue"),
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
  x
){
  if (is.character(x)){
    x <- get_logger(x)
  } else {
    assert(is_Logger(x))
  }

  !length(x$appenders) &
  !length(x$filters) &
  isTRUE(x$propagate) &
  identical(x$exception_handler, default_exception_handler) &
  is.null(x$.__enclos_env__$private$.threshold)
}
