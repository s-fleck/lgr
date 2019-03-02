logger_config <- function(x){
  assert(is.list(x))

  assert(all(
    names(x) %in% c("exception_handler", "propagate", "threshold", "appenders", "filters")
  ))

  assert(is.function(x$exception_handler))
  assert(is_scalar_bool(x$propagate))

  x$threshold <- standardize_threshold(x$threshold)
  x$filters   <- standardize_filters_list(x$filters)
  x$appenders <- standardize_appenders_list(x$appenders)

  class(x) <- c("logger_config", "list")
  x
}




as_logger_config <- function(
  x
){
  UseMethod("as_logger_config")
}



as_logger_config.character <- function(
  x
){
  assert_namespace("yaml")

  xe <- eval(substitute(x))

  if (length(x) > 1 || !grepl("\n", x)){
    dd <- yaml::read_yaml(file = x)
  } else {
    dd <- yaml::read_yaml(text = x)
  }

  assert(
    identical(length(names(dd)), 1L),
    "If 'x' is a yaml file, it must contain a single logger object"
  )

  root <- get0_R6Class(names(dd))
  assert(
    is_Logger(root),
    "`", xe, "` is must start with a single Logger object.",
    "See ?load_logger_config for examples"
  )
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
