# sfmisc utils 1.0.0.9034




# utils -------------------------------------------------------------------

# nocov start
# commonly used utility functions included from the package sfmisc


#' Paste and Truncate
#'
#' @param x a vector
#' @param width (maximum) width of result
#' @param dots `character` scalar. String to use for ellipses
#' @inheritParams paste
#'
#' @return a `character` scalar
#' @noRd
#'
#' @examples
#'   ptrunc(month.abb)
#'   ptrunc(month.abb, month.name)
#'
ptrunc <- function(
  ...,
  width = 40L,
  sep = ", ",
  collapse = ", ",
  dots = " ..."
){
  assert(width > 7L, "The minimum supported width is 8")
  x <- paste(..., sep = sep, collapse = collapse)

  sel <- vapply(x, nchar, integer(1), USE.NAMES = FALSE) > width

  x[sel] <- strtrim(x[sel], width = width - 4L)
  x[sel] <- paste0(gsub(",{0,1}\\s*$", "", x[sel]), dots)
  x
}




fmt_class <- function(x, open = "<", close = ">"){
  paste0(open, paste(x, collapse = "/"), close)
}




#' @param x any \R object
#' @param ignore subclasses to ignore
#' @noRd
class_fmt <- function(x, ignore = NULL){
  fmt_class(setdiff(class(x), ignore))
}




compact <- function(x){
  x[!vapply(x, is.null, FALSE)]
}




walk <- function(.x, .f, ...){
  for (i in seq_along(.x)){
    .f(.x[[i]], ...)
  }

  invisible(.x)
}




# assertions --------------------------------------------------------------

#' Assert a condition
#'
#' A simpler and more efficient for [base::stopifnot()] that has an easy
#' mechanism for supplying custom error messages. As opposed to `stopifnot()`,
#' `assert()` only works with a single (scalar) assertions.
#'
#' @param cond `TRUE` or `FALSE` (without any attributes). `FALSE` will throw
#'   an exception with an automatically constructed error message (if `...`
#'   was not supplied). Anything else will throw an exception stating that
#'   `cond` was not valid.
#' @param ... passed on to [stop()]
#' @param call. passed on to [stop()]
#' @param domain passed on to [stop()]
#'
#' @noRd
#'
#' @return TRUE on success
#'
#' @examples
#'
#' \dontrun{
#' assert(1 == 1)
#' assert(1 == 2)
#' }
#'
#'
assert <- function(
  cond,
  ...,
  call. = FALSE,
  domain = NULL
){
  if (identical(cond, TRUE)){
    return(TRUE)
  } else if (identical(cond, FALSE)){
    if (identical(length(list(...)), 0L)){
      msg <- paste0("`", deparse(match.call()[[2]]), "`", " is not 'TRUE'")
      stop(msg, call. = call., domain = domain)
    } else {
      suppressWarnings( stop(..., call. = call., domain = domain) )
    }

  } else {
    stop("Assertion must be either 'TRUE' or 'FALSE'")
  }
}




assert_namespace <- function(...){
  pkgs <- c(...)

  res <- vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)
  if (all(res)){
    return(invisible(TRUE))

  } else {
    miss <- pkgs[!res]

    if (identical(length(miss), 1L)){
      msg <- sprintf(paste(
        "This function requires the package '%s'. You can install it with",
        '`install.packages("%s")`.'), miss, miss
      )
    } else {
      msg <- sprintf(
        paste(
          "This function requires the packages %s. You can install them with",
          "`install.packages(%s)`."
        ),
        paste(miss, collapse = ", "),
        paste0("c(", paste(paste0('\"', miss, '\"'), collapse = ", "), ")")
      )
    }
  }

  stop(msg, call. = FALSE)
}




# predicates --------------------------------------------------------------



is_error <- function(x){
  inherits(x, "error")
}




is_try_error <- function(x){
  inherits(x, "try-error")
}




is_scalar <- function(x){
  identical(length(x), 1L)
}




is_POSIXct <- function(x){
  inherits(x, "POSIXct")
}




is_scalar_POSIXct <- function(x){
  is_POSIXct(x) && is_scalar(x)
}




is_POSIXlt <- function(x){
  inherits(x, "POSIXlt")
}




is_scalar_POSIXlt <- function(x){
  is_POSIXlt(x) && is_scalar(x)
}




is_POSIXt <- function(x){
  inherits(x, "POSIXt")
}




is_scalar_POSIXt <- function(x){
  is_POSIXt(x) && is_scalar(x)
}




is_Date <- function(x){
  inherits(x, "Date")
}




is_scalar_Date <- function(x){
  is_Date(x) && is_scalar(x)
}




is_scalar_list <- function(x){
  is_list(x) && is_scalar(x)
}




is_scalar_atomic <- function(x){
  is.atomic(x) && is_scalar(x)
}




is_scalar_logical <- function(x){
  is.logical(x) && is_scalar(x)
}




is_scalar_integer <- function(x){
  is.integer(x) && is_scalar(x)
}




is_scalar_factor <- function(x){
  is.factor(x) && is_scalar(x)
}




is_scalar_list <- function(x){
  is.list(x) && is_scalar(x)
}




is_scalar_numeric <- function(x){
  is.numeric(x) && is_scalar(x)
}




is_scalar_character <- function(x){
  is.character(x) && is_scalar(x)
}




is_vector <- function(x){
  is.atomic(x) || is.list(x)
}




is_bool <- function(x){
  is.logical(x) && !anyNA(x)
}




#' Check if Object is a Boolean
#'
#' Check wheter an object is either `TRUE` or `FALSE`.
#'
#' @param x Any \R Object.
#' @return either `TRUE` or `FALSE`
#' @noRd
#'
is_scalar_bool <- function(x){
  identical(x, TRUE) || identical(x, FALSE)
}




#' Check if Object is Integer-like
#'
#' Check wheter an object is either `TRUE` or `FALSE`.
#'
#' @param x Any \R Object.
#' @return either `TRUE` or `FALSE`
#' @noRd
#'
is_integerish <- function(x){
  if (!is.numeric(x)){
    FALSE
  } else {
    all(as.integer(x) == x)
  }
}




is_scalar_integerish <- function(x){
  is_scalar(x) && is_integerish(x)
}




is_n <- function(x){
  is_scalar_integerish(x) && identical(x > 0, TRUE)
}




is_n0 <- function(x){
  is_scalar_integerish(x) && identical(x >= 0, TRUE)
}




#' Check if Objects have the same length
#'
#' @param ... Any number of \R Objects.
#'
#' @return either `TRUE` or `FALSE`
#' @noRd
is_equal_length <- function(...){
  lengths <- vapply(list(...), length, 1L)
  identical(length(unique(lengths)), 1L)
}




#' Check if Object has length 0
#'
#' Check wheter an object is either `TRUE` or `FALSE`.
#'
#' @param x Any \R Object.
#' @return either `TRUE` or `FALSE`
#' @noRd
#'
is_empty <- function(x){
  identical(length(x), 0L)
}




#' Check if a String is Blank
#'
#' Check wheter a character vector contains only of spaces
#'
#' @param x Any \R Object.
#' @return either `TRUE` or `FALSE`
#' @noRd
#'
is_blank <- function(x){
  trimws(x) == ""
}




#' Test if a Vector or Combination of Vectors is a Candidate Key
#'
#' Checks if all elements of the atomic vector `x`, or the combination of
#' all elements of `x` if `x` is a `list`, are unique and neither `NA` or
#' `infinite`.
#'
#' @param x a atomic vector or a list of atomic vectors
#'
#' @return `TRUE/FALSE`
#' @noRd
#'
#' @examples
#'
#' is_candidate_key(c(1, 2, 3))
#' is_candidate_key(c(1, 2, NA))
#' is_candidate_key(c(1, 2, Inf))
#'
#' td <- data.frame(
#'   x = 1:10,
#'   y = 1:2,
#'   z = 1:5
#' )
#'
#' is_candidate_key(list(td$x, td$z))
#' # a data.frame is just a special list
#' is_candidate_key(td[, c("y", "z")])
is_candidate_key <- function(x){

  if (is.atomic(x)){
    # !is.infinite instead of is.finite because x can be a character vector
    length(x) > 1 &&
    all(!is.infinite(x)) &&
    !any(is.na(x)) &&
    identical(length(unique(x)), length(x))
  } else if (is.list(x)){
    length(x) > 0 &&
    length(x[[1]] > 0) &&
    do.call(is_equal_length, x) &&
    all(vapply(x, function(.x) all(!is.infinite(.x)), logical(1))) &&
    all(vapply(x, function(.x) !any(is.na(.x)), logical(1))) &&
    !any(duplicated(as.data.frame(x)))
  }
}




# https://modern-sql.com/feature/is-distinct-from
is_not_distinct_from <- function(x, y){
  ((x == y) & !is.na(x) & !is.na(y)) | (is.na(x) & is.na(y))
}




is_distinct_from <- function(x, y){
  ((x != y) & !is.na(x) & !is.na(y)) | (is.na(x) != is.na(y))
}




is_windows_path <- function(x){
  nchar(x) >= 2 & grepl("^[A-Za-z].*", x) & substr(x, 2, 2) == ":"
}



# equalish ----------------------------------------------------------------

#' Check for equality within a tolerance level
#'
#'
#'
#' @param x,y `numeric` vectors
#' @param tolerance `numeric` scalar. tolerance level (absolute value). Defaults
#'   to `.Machine$double.eps^0.5` which is a sensible default for comparing
#'   floating point numbers.
#'
#' @return `equalish()` returns TRUE if the absolute difference between `x` and
#'   `y` is less than `tolerance`.
#' @noRd
#' @seealso [.Machine]
#'
#'
#' @examples
#' a <- 0.7
#' b <- 0.2
#' a - b == 0.5
#' equalish(a - b, 0.5)
#'
equalish <- function(x, y, tolerance = .Machine$double.eps ^ 0.5){
  assert(is_scalar_numeric(tolerance) && tolerance >= 0)
  abs(x - y) < tolerance
}




#' @return `equalish_frac()` returns `TRUE` if the relative difference between
#'   `x` and `y` is smaller than `tolerance`. The relative difference is
#'   defined as `abs(x - y) / pmax(abs(x), abs(y))`. If both `x` and `y` are
#'   `0` the relative difference is not defined, but this function will still
#'   return `TRUE`.
#'
#' @noRd
#' @examples
#'
#' equalish_frac(1000, 1010, tolerance = 0.01)
#' equalish_frac(1000, 1010, tolerance = 0.009)
#' equalish_frac(0, 0)
#'
equalish_frac <- function(x, y, tolerance = .Machine$double.eps ^ 0.5){
  assert(is_scalar_numeric(tolerance) && tolerance >= 0)
  res <- abs(x - y) / pmax(abs(x), abs(y)) < tolerance
  res[x == 0 & y == 0] <- TRUE
  res
}




# all_are -----------------------------------------------------------------

#' Convert vector if identical elements to scalar
#'
#' Returns `unique(x)` if all elements of `x` are identical, throws an error if
#' not.
#'
#' @inheritParams all_are_identical
#'
#' @return A scalar of the same type as `x`
#' @noRd
as_scalar <- function(x){
  res <- unique(x)
  if (is_scalar(res)){
    return(res)
  } else {
    stop("Not all elements of x are identical")
  }
}




#' Test if all elements of a vector are identical
#'
#' @param x any object that can be handled by [unique()] (usually a vector or
#'   list)
#' @param empty_value Value to return if function is called on a vector of
#'   length 0 (e.g. `NULL`, `numeric()`, ...)
#'
#' @noRd
#' @family special equality checks
#' @return `TRUE/FALSE`
#'
#' @examples
#'
#' all_are_identical(c(1,2,3))
#' all_are_identical(c(1,1,1))
#'
all_are_identical <- function(x, empty_value = FALSE) {
  assert(length(empty_value) <= 1)

  if (length(x) > 0L) {
    return(identical(length(unique(x)), 1L))

  } else {

    if (is.null(x)){
      warning("'x' is NULL")
    } else {
      warning("'x' is an empty vector")
    }

    return(empty_value)
  }
}




#' Test if all elements of a vector are unique
#'
#' @inheritParams all_are_identical
#'
#' @return TRUE/FALSE
#'
#' @noRd
#' @family special equality checks
#'
#' @examples
#'
#' all_are_identical(c(1,2,3))
#' all_are_identical(c(1,1,1))
#'
all_are_distinct <- function(
  x,
  empty_value = FALSE
){
  assert(length(empty_value) <= 1)

  if (identical(length(x), 1L)) {
    return(TRUE)

  } else if (length(x) > 1L) {
    return(identical(length(unique(x)), length(x)))

  } else {

    if (is.null(x)){
      warning("'x' is NULL")
    } else {
      warning("'x' is an empty vector")
    }

    return(empty_value)
  }
}




n_distinct <- function(x){
  length(unique(x))
}




# misc --------------------------------------------------------------------


pad_left <- function(
  x,
  width = max(nchar(paste(x))),
  pad = " "
){
  diff <- pmax(width - nchar(paste(x)), 0L)
  padding <-
    vapply(diff, function(i) paste(rep.int(pad, i), collapse = ""), character(1))
  paste0(padding, x)
}




pad_right <- function(
  x,
  width = max(nchar(paste(x))),
  pad = " "
){
  diff <- pmax(width - nchar(paste(x)), 0L)
  padding <-
    vapply(diff, function(i) paste(rep.int(pad, i), collapse = ""), character(1))
  paste0(x, padding)
}




`%||%` <- function(x, y){
  if (is.null(x))
    y
  else (x)
}




preview_object <- function(
  x,
  width = 32,
  brackets = c("(", ")"),
  quotes   = c("`", "`"),
  dots = ".."
){
  if (is.function(x)){
    fmls <- names(formals(x))
    len_fmls <- length(fmls)

    if (len_fmls > 4){
      fmls <- fmls[1:4]
      fmls_fmt <- paste(fmls, collapse = ", ")
      fmls_fmt <- paste0(fmls_fmt, ", +", len_fmls - length(fmls), "")
    } else {
      fmls_fmt <- paste(fmls, collapse = ", ")
    }
    return(fmt_class(paste(
      fmt_class(class(x), open = "", close = ""), "(", fmls_fmt, ")",
      sep = ""
    )))
  }


  if (!is.atomic(x))
    return(class_fmt(x))

  if (is.numeric(x))
    x <- format(x, justify = "none", drop0trailing = TRUE, trim = TRUE)

  res <- ptrunc(x, collapse = ", ", width = width, dots = dots)

  if (length(x) > 1)
    res <- paste0(brackets[[1]], res, brackets[[2]])
  else
    res <- paste0(quotes[[1]], res, quotes[[2]])

  res
}




#' Collapse text vectors with a comma
#'
#' @param x `character` vector
#'
#' @return a `character` scalar
#' @noRd
comma <- function(..., collapse = ", "){
  paste(unlist(c(...)), collapse = collapse)
}




#' Collapse text vectors with a comma (no duplicates)
#'
#' @param x `character` vector
#'
#' @return a `character` scalar
#' @noRd
commaset <- function(..., collapse = ", "){
  paste(sort(unique(unlist(c(...)))), collapse = collapse)
}




#' Clean up paths to make them comparable, inspired by fs::path_tidy
#'
#' @param x `character` vector
#'
#' @return a `character` vector
#' @noRd
path_tidy <- function(x){
  x <- gsub("\\\\", "/", x)
  x <- gsub("(?!^)/+", "/", x, perl = TRUE)

  sel <- x != "/"
  x[sel] <- gsub("/$", "", x[sel])

  sel <- is_windows_path(x)

  if (any(sel)){
    clean_win <- function(.x){
      substr(.x, 1, 1)  <- toupper(substr(.x, 1 ,1))
      .sel <- nchar(.x) == 2
      .x[.sel] <- paste0(.x[.sel], "/")
      .x
    }

    x[sel] <- clean_win(x[sel])
  }

  x
}




#' Return (unique) duplicated elements of a vector or rows of a data.frame
#'
#' For every element/row of `x` that has at least one duplicate, return one
#' instance of that element.
#'
#' @param x an [atomic] vector or [data.frame]
#' @param ... passed on to [duplicated()]
#'
#' @noRd
#'
#' @examples
#' dupes(c(1, 1, 1, 2))
#' dupes(cars[c(1, 1, 1, 2), ])
dupes <- function(x, ...){

  if (is.atomic(x)){
    sort(unique(x[duplicated(x, ...)]))
  } else if (is.data.frame(x)){
    res <- unique(x[duplicated(x, ...), ])
    row.names(res) <- NULL
    res
  }
}

# nocov end
