#' Single Line Summary
#' @return a `character` scalar
#' @noRd
sls <- function(x){
  UseMethod("sls")
}

sls.Appender <- function(x){
  threshold <- x$threshold
  class(x)[[1]]
}


sls.default <- function(x){
  if (is.atomic(x)){
    ptrunc(x, width = 64)
  } else {
    class_fmt(x)
  }
}
