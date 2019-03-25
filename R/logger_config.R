#' Logger Configuration Objects
#'
#' `logger_config()` is an S3 constructor for `logger_config` objects
#' that can be passed to the `$config` method of a [Logger]. You
#' can just pass a normal `list` instead, but using this constructor is
#' a more formal way that includes additional argument checking.
#'
#'
#' @param appenders see [Logger]
#' @param threshold see [Logger]
#' @param filters see [Logger]
#' @param exception_handler see [Logger]
#' @param propagate see [Logger]
#'
#' @return a `list` with the subclass `"logger_config"`
#' @export
#'
#' @examples
#' lg <- get_logger("test")
#'
#' # explicetely defining logger configurations with logger_config
#'
#' # call without arguments to generate the default configuration
#' cfg <- logger_config()  # same as the unconfigured state of a Logger
#' lg$config(cfg)
#'
#'
#' # Creating a logger config form YAML
#' cfg <- "
#' Logger:
#'   name: test/blubb
#'   threshold: info
#'   propagate: false
#'   appenders:
#'     AppenderFile:
#'       file: /tmp/testlog.txt
#' "
#' lg$config(cfg)  # calls as_logger_config() internally
#' lg$config(logger_config())  # reset logger config to default state
logger_config <- function(
  appenders = list(),
  threshold = NULL,
  filters = list(),
  exception_handler = default_exception_handler,
  propagate = TRUE
){
  assert(is.function(exception_handler))
  assert(is_scalar_bool(propagate))

  if (!is.null(threshold)){
    threshold <- standardize_threshold(threshold)
  }

  structure(
    list(
      appenders = standardize_appenders_list(appenders),
      threshold = threshold,
      filters   = standardize_filters_list(filters),
      exception_handler = exception_handler,
      propagate = propagate
    ),
    class = c("logger_config", "list")
  )
}



#' `as_logger_config()` coerces any supported \R object to a `logger_config`
#' object. You usually do not have to call this function directly, as it is
#' automatically invoked by the `config()` method of Loggers (see examples)
#'
#' @param x any \R object. Especially:
#'   * A `character` scalar. This can either be the path to a
#'     YAML file or a character scalar containing valid YAML
#'   * a list containing the elements `appenders`, `threshold`, `exception_handler`,
#'     `propagate` and `filters`. See the section *Fields* in [Logger] for
#'     details.
#'   * a Logger object, to clone its configuration.
#'
#' @rdname logger_config
#' @seealso \url{https://yaml.org/}
#' @return a logger_config object
#' @export
#'
as_logger_config <- function(x){
  UseMethod("as_logger_config")
}




#' @rdname logger_config
#' @export
as_logger_config.list <- function(x){
  assert(is.list(x))

  assert(setequal(
    names(x), c("exception_handler", "propagate", "threshold", "appenders", "filters")
  ))

  logger_config(
    threshold = x$threshold,
    filters = x$filters,
    appenders = x$appenders,
    exception_handler = x$exception_handler,
    propagate = x$propagate
  )
}




#' @rdname logger_config
#' @export
as_logger_config.character <- function(
  x
){
  assert_namespace("yaml")

  if (length(x) > 1 || !grepl("\n", x)){
    dd <- yaml::read_yaml(file = x)
  } else {
    dd <- yaml::read_yaml(text = x)
  }

  assert(
    identical(length(names(dd)), 1L),
    "If 'x' is a YAML file, it must contain a single logger object"
  )

  res <- resolve_r6_ctors(dd)

  as_logger_config(res)
}




#' @export
#' @rdname logger_config
as_logger_config.Logger <- function(x){
  logger_config(
    appenders = x$appenders,
    threshold = x$threshold,
    exception_handler = x$exception_handler,
    filters = x$filters,
    propagate = x$propagate
  )
}




resolve_r6_ctors <- function(x){

  ctors <- lapply(names(x), get0_R6Class)

  for (i in seq_along(x)){
    if (length(ctors) && !is.null(ctors[[i]])){

      args <- resolve_r6_ctors(x[[i]])
      if (is.null(args)) args <- list()

      # Allow user to supply the layout directly without having to specify
      # the layout: key manually for Appenders
      if ("Appender" %in% deparse(ctors[[i]]$inherit)){
        if (!"layout" %in% names(args)){
          for (j in rev(seq_along(args))){
            if (inherits(args[[j]], "Layout")){
              args$layout <- args[[j]]
              args[[j]] <- NULL
              break
            }
          }
        }
      }

      # prevent logger not named warning
      suppressWarnings(x[[i]] <- do.call(ctors[[i]]$new, args))
    } else {
      if (is.recursive(x[[i]])){
        x[[i]] <- resolve_r6_ctors(x[[i]])
      } else {
        x[[i]] <- x[[i]]
      }
    }
  }


  if (is_scalar(x) && R6::is.R6(x[[1]])){
    x[[1]]
  } else {
    x
  }
}




get0_R6Class <- function(x){
  assert(is_scalar_character(x))
  res <- get0(x, envir = parent.env(environment()))

  if (R6::is.R6Class(res)){
    res
  } else {
    NULL
  }
}




standardize_appenders_list <- function(x){
  if (is.null(x))
    return(list())

  if (inherits(x, "Appender"))
    x <- list(x)

  assert(
    is.list(x) && all(vapply(x, inherits, TRUE, "Appender")),
    "'appenders' must either be a single Appender, a list thereof, or ",
    "NULL for no appenders."
  )

  x
}




standardize_filters_list <- function(x){
  if (is.null(x))
    return(list())

  if (is_filter(x))
    return(list(x))

  assert(
    is.list(x) && all(vapply(x, is_filter, logical(1))),
    "'filters' must be a list Filters or of functions with the single argument",
    "'event'"
  )

  x
}
