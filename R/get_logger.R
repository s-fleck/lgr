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
#'   Logger references created before the reset call (see examples). The
#'   root logger is the exception: since it is a package-level singleton
#'   (`lgr`), `get_logger("root", reset = TRUE)` resets it in place (exactly
#'   like `lgr$config(NULL)`) rather than replacing the object, so existing
#'   `lgr` references remain valid.
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
#' try(lg$info("lg has been invalidated and no longer works"))
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

  if (identical(name, "root")){
    # The root Logger is a package-level singleton (also bound to `lgr`).
    # Unlike other Loggers it is never destroyed/recreated: `reset` resets
    # its fields in place (like `$config(NULL)`) instead of swapping in a
    # new object, which would desync the `lgr` binding from `loggers$root`
    # and leave existing `lgr` references pointing at a stale, orphaned
    # Logger. Read from `loggers` directly (not the `lgr` binding) because
    # `.onLoad()` calls `get_logger("root")` to *create* the `lgr` binding,
    # before it exists.
    assert(identical(class, Logger), "The root logger cannot be a `LoggerGlue`.")
    res <- get0("root", envir = loggers, inherits = FALSE)
    if (reset && !is.null(res)) res$config(NULL)
    return(res)
  }

  res <- get0(name, envir = loggers, inherits = FALSE)

  if (reset && is_Logger(res)){
    res$set_filters(function(event) stop(call. = FALSE, sprintf(paste(
      "Trying to log via a Logger reference that is no longer valid.",
      "Logger references become invalid when you reset a",
      "Logger with `get_logger(reset = TRUE)`. Please",
      "re-create the Logger reference with `get_logger(%s)`"), name
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
      name
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
