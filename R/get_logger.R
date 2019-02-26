loggers <- new.env()




#' Get/Create a Logger
#'
#' @param name a `character` scalar or vector: The qualified name of the Logger
#'   as a hierarchical value.
#' @param class An [R6ClassGenerator][R6::R6] object. Usually `Logger` or `LoggerGlue`
#'   are the only valid choices.
#'
#' @return a [Logger]
#' @export
#'
#' @examples
#' lg <- get_logger("log/ger/test")
#' # equivalent to
#' lg <- get_logger(c("log", "get", "test"))
#' lg$warn("a %s message", "warning")
#' lg
#' lg$parent
#'
#' lg <- get_logger_glue("log/ger")
#' lg$warn("a {.text} message", .text = "warning")
get_logger <- function(
  name,
  class = Logger
){
  if (missing(name) || !length(name) || all(is_blank(name))){
    return(lgr::lgr)
  }
  nm_cur <- unlist(strsplit(name, "/", fixed = TRUE))
  name   <- paste(nm_cur, collapse = "/")
  res    <- get0(name, envir = loggers)

  if (inherits(res, class$classname)){
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
    get_logger(name = name, class = LoggerGlue)
  } else {
    warning("Logger '", name, "' already exists.")
  }
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
