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
#'
logger_config <- function(
  appenders = NULL,
  threshold = NULL,
  filters = NULL,
  exception_handler = NULL,
  propagate = TRUE
){
  # init/preconditions
  if (is.function(exception_handler)){
    exception_handler <- deparse(exception_handler)

  } else if (!is.null(exception_handler)){
    assert(
      is.character(exception_handler),
      "'exception_handler' must be a function, a character scalar giving the",
      "name of a function, or a character vector containing arbitrary R code."
    )
  }

  assert(is.null(threshold) || is_threshold(threshold))
  assert(is.null(appenders) || all(vapply(appenders, is.list, logical(1))))
  assert(is.null(filters)   || all(vapply(filters,   is.list, logical(1))))

  if (!is.null(propagate)){
    assert(is_scalar(propagate))
    if (is.character(propagate))  propagate <- toupper(propagate)
    propagate <- as.logical(propagate)
    assert(is_scalar_bool(propagate))
  }



  cfg <- compact(list(
    appenders = appenders,
    threshold = threshold,
    filters   = filters,
    exception_handler = exception_handler,
    propagate = propagate
  ))

  class(cfg) <- c("logger_config", "list")
  cfg
}




is_logger_config <- function(x){
  inherits(x, "logger_config")
}




parsed_logger_config  <- function(
  appenders = list(),
  threshold = NULL,
  filters = list(),
  exception_handler = default_exception_handler,
  propagate = TRUE
){
  structure(
    list(
      appenders = standardize_appenders_list(appenders),
      threshold = threshold,
      filters   = standardize_filters_list(filters),
      exception_handler = exception_handler,
      propagate = propagate
    ),
    class = c("parsed_logger_config", "list")
  )
}




is_parsed_logger_config <- function(x){
  inherits(x, "parsed_logger_config")
}




parse_logger_config <- function(
  x,
  defaults = parsed_logger_config()
){
  if (is_parsed_logger_config(x)){
    return(x)
  } else {
    assert(all(names(x) %in% names(defaults)))
  }


  if (is_logger_config(x)){
    objects <- resolve_r6_ctors(x)

    res <- defaults

    if ("appenders" %in% names(x))
      res$appenders <- standardize_appenders_list(objects$appenders)

    if ("exception_handler" %in% names(x))
      res$exception_handler <- eval(parse(text = x[["exception_handler"]]))

    if ("propagate" %in% names(x))
      res$propagate <- as.logical(toupper(x[["propagate"]]))

    if ("threshold" %in% names(x))
      res$threshold <- standardize_threshold(x$threshold)

    if ("filters" %in% names(x))
      res$filters <- standardize_filters_list(objects$filters)

    class(res) <-  c("parsed_logger_config", "list")

  } else {
    res <- do.call(parsed_logger_config, x)

  }

  res
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




#' @export
as_logger_config.NULL <- function(x){
  as_logger_config(list())
}




#' @rdname logger_config
#' @export
as_logger_config.list <- function(x){
  if (identical(names(x), "Logger"))
    x <- x[["Logger"]]

  assert(all(
    names(x) %in% c("exception_handler", "propagate", "threshold", "appenders", "filters")
  ))
  class(x) <- c("logger_config", class(x))
  x
}




#' @rdname logger_config
#' @export
as_logger_config.character <- function(
  x
){
  if (identical(length(x), 1L) && !grepl("\n", x)){
    if (
      identical(tolower(tools::file_ext(x)), "json") &&
      requireNamespace("jsonlite", quietly = TRUE)
    ){
      # if jsonliste is installed use jsonlite for yaml, otherwiese use the
      # yaml package (since JSON is also valid YAML)
      dd <- jsonlite::read_json(x, simplifyVector = TRUE)

    } else {
      assert(file.exists(x), "The file '", x, "' does not exist.")
      assert_namespace("yaml")
      dd <- yaml::read_yaml(file = x)
    }
  } else {
    dd <- yaml::read_yaml(text = x)
  }

  assert(
    identical(names(dd), "Logger"),
    "If 'x' is a YAML file, it must contain a single logger object"
  )

  as_logger_config(dd)
}




resolve_r6_ctors <- function(x){
  ctors <- lapply(names(x), get0_R6Class)

  for (i in seq_along(x)){
    if (length(ctors) && !is.null(ctors[[i]])){

      args <- resolve_r6_ctors(x[[i]])
      if (is.null(args)) args <- list()

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

  x
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
