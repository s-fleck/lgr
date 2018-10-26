# sfmisc utils 0.0.1.9011




# utils -------------------------------------------------------------------

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




assert_namespace <- function(x){
  assert(requireNamespace(x, quietly = TRUE))
  invisible(TRUE)
}




# conditions --------------------------------------------------------------

#' Condition constructor
#'
#' A constructur function for conditions, taken from
#' \url{http://adv-r.had.co.nz/beyond-exception-handling.html}
#'
#' @param subclass Subclass to assign to the condition
#' @param message  message to be passed to the condition
#' @param call     call passed on to the conditon
#' @param ...      further list elements to be passed on to the resulting object
#'
#' @return a condition object
#' @noRd
#'
#' @examples
#'
#' \dontrun{
#' # Construct a custom condition
#' malformed_log_entry_error <- function(text) {
#'   msg <- paste0("Malformed log entry: ", text)
#'   condition(
#'     c("malformed_log_entry_entry", "error"),
#'     message = msg,
#'     text = text
#'   )
#' }
#'
#'
#' # Signal the condition
#' parse_log_entry <- function(text) {
#'   if (!well_formed_log_entry(text)) {
#'     stop(malformed_log_entry_error(text))
#'    }
#' }
#'
#'
#' # Handle the condition
#' tryCatch(
#'   malformed_log_entry = function(e) NULL,
#'   parse_log_entry(text)
#' )
#' }
#'
condition <- function(subclass, message, call = sys.call(-1), ...) {
  structure(
    class = c(subclass, "condition"),
    list(message = message, call = call, ...)
  )
}




error <- function(subclass, message, call = sys.call(-1), ...) {
  structure(
    class = c(subclass, "error", "condition"),
    list(message = message, call = call, ...)
  )
}




# predicates --------------------------------------------------------------
is_scalar <- function(x){
  identical(length(x), 1L)
}




is_scalar_character <- function(x){
  is.character(x) && is_scalar(x)
}




is_scalar_logical <- function(x){
  is.logical(x) && is_scalar(x)
}




is_scalar_integerish <- function(x){
  is_scalar(x) && is_integerish(x)
}




is_tf <- function(x){
  is.logical(x) && !anyNA(x)
}




is_scalar_tf <- function(x){
  identical(x, TRUE) || identical(x, FALSE)
}




is_integerish <- function(x){
  if (!is.numeric(x)){
    FALSE
  } else {
    all(as.integer(x) == x)
  }
}




is_equal_length <- function(...){
  lengths <- vapply(list(...), length, 1L)
  identical(length(unique(lengths)), 1L)
}




is_empty <- function(x){
  identical(length(x), 0L)
}




is_blank <- function(x){
  trimws(x) == ""
}




# all_are -----------------------------------------------------------------


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
