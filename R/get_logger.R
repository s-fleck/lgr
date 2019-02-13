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
  name,
  ...
){
  nm_cur <- unlist(strsplit(name, "/", fixed = TRUE))
  name   <- paste(nm_cur, collapse = "/")
  res    <- get0(name, envir = loggers)

  if (!is.null(res)){
    return(res)
  }

  if (length(nm_cur) > 1){
    parent <- get_logger(nm_cur[-length(nm_cur)])
  } else {
    parent <- get_logger("root")
  }

  assign(
    name,
    Logger$new(name, parent = parent),
    envir = loggers
  )

  get_logger(name)
}



exists_logger <- function(
  name
){
  inherits(get0(name, envir = loggers), "Logger")
}
