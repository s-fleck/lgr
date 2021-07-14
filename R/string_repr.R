#' Short string representation for R objects
#'
#' This is inspired by the python function `repr` and produces a short
#' string representation of any \R object that is suitable for logging and error
#' messages. It is a generic so you can implement methods for custom S3 objects.
#'
#' @param x Any \R object.
#' @param width a scalar integer
#' @param ... passed on to methods
#'
#' @return a `scalar` character
#' @export
#'
#' @examples
#' string_repr(iris)
#' string_repr(LETTERS)
#' string_repr(LETTERS, 10)
string_repr <- function(
  x,
  width = 32,
  ...
){
  assert(is_scalar_integerish(width))

  if (width < 8){
    warning("string_repr() does not support width < 8")
    width <- 8L
  }
  UseMethod("string_repr")
}




#' @rdname string_repr
#' @export
string_repr.function <- function(
  x,
  width = 32L,
  ...
){
  fmls <- names(formals(x))
  len_fmls <- length(fmls)

  if (len_fmls > 4){
    fmls <- fmls[1:4]
    fmls_fmt <- paste(fmls, collapse = ", ")
    fmls_fmt <- paste0(fmls_fmt, ", +", len_fmls - length(fmls), "")
  } else {
    fmls_fmt <- paste(fmls, collapse = ", ")
  }

  fmt_class(paste(
    fmt_class(class(x), open = "", close = ""), "(", fmls_fmt, ")",
    sep = ""
  ))
}


#' @rdname string_repr
#' @export
string_repr.data.frame <- function(
  x,
  width = 32L,
  ...
){
  x_class <- fmt_class(class(x), open = "", close ="")
  x_shape <- paste0(nrow(x),"x", ncol(x))

  if (nchar(x_class) + nchar(x_shape) + 3L <= width){
    res <- paste0("<", x_class, " ", x_shape, ">")
  } else {
    if (nchar(x_class) >= width - 2L){
      x_class <- paste0(strtrim(x_class, width - 4L), "..")
    }
    res <- paste0("<", strtrim(x_class, width = width - 2L), ">")
  }
  res
}


#' @rdname string_repr
#' @export
string_repr.matrix <- string_repr.data.frame


string_repr.numeric <- function(
  x,
  width = 32L,
  ...
){
  string_repr(format(x, justify = "none", drop0trailing = TRUE, trim = TRUE))
}


#' @rdname string_repr
#' @export
string_repr.default <- function(
  x,
  width = 32L,
  ...
){
  if (is.recursive(x)){
    x_class <- fmt_class(class(x), open = "", close ="")

    if (nchar(x_class) >= width - 2L){
      x_class <- paste0(strtrim(x_class, width - 4L), "..")
    }
    res <- paste0("<", strtrim(x_class, width = width - 2L), ">")

  } else {
    res <- ptrunc(x, collapse = ", ", width = width, dots = "..")

    if (is_scalar(x)){
      res <- paste0("`", res, "`")
    } else {
      res <- paste0("(", res, ")")
    }
  }

  res
}
