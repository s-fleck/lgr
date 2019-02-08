loggers <- new.env()




#' Title
#'
#' @param logger
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_logger <- function(
  logger,
  ...
){

  res <- get0(logger, envir = loggers)

  if (!is.null(res)){
    return(res)

  }

  names <- unlist(strsplit(logger, ".", fixed = TRUE))

  if (identical(length(names), 1L)){
    assign(logger, Logger$new(logger, ...), envir = loggers)
    return(get(logger, envir = loggers))

  } else {
    get_logger(
      names[length(names)],
      parent = get_logger(names[length(names) - 1L])
    )
  }
}
