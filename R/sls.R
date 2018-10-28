#' Single Line Summary
#' @return a `character` scalar
#' @noRd
sls <- function(x, colors = FALSE){
  UseMethod("sls")
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
sls.Appender <- function(x, colors = FALSE){
  x$format(colors = colors, single_line_summary = TRUE)
}


#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
sls.default <- function(x, colors = FALSE){
  if (is.atomic(x)){
    ptrunc(x, width = 64)
  } else {
    class_fmt(x)
  }
}


#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
sls.log_levels <- function(x, colors = FALSE){
  paste0(names(x), " (", x, ")", collapse = ", ")
}
