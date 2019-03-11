as_logger_config <- function(x){
  UseMethod("as_logger_config")
}




as_logger_config.list <- function(x){
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

  if (length(x) > 1 || !grepl("\n", x)){
    dd <- yaml::read_yaml(file = x)
  } else {
    dd <- yaml::read_yaml(text = x)
  }

  assert(
    identical(length(names(dd)), 1L),
    "If 'x' is a yaml file, it must contain a single logger object"
  )

  res <- resolve_r6_ctors(dd)

  assert(
    is_Logger(res),
    "If `x` is a yaml file or string, it must contain a single logger object"
  )

  res
}




resolve_r6_ctors <- function(x){

  ctors <- lapply(names(x), get0_R6Class)


  for (i in seq_along(x)){
    if (length(ctors) && !is.null(ctors[[i]])){

      args <- resolve_r6_ctors(x[[i]])

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

      x[[i]] <- do.call(ctors[[i]]$new, args)
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
