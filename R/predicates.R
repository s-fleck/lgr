is_valid_log_levels <- function(
  x,
  log_levels = getOption("yog.log_levels")
){
  is.atomic(x) &&
    (is.na(x) || is_integerish(x) || x %in% names(log_levels))
}




assert_valid_log_levels <- function(
  x,
  log_levels = getOption("yog.log_levels")
){
  if (identical(is_valid_log_levels(x), TRUE))  return(TRUE)

  if (is.atomic(x)){
    x <- paste0("'", ptrunc(x, width = 32), "'")
  } else {
    x <- class_fmt(x)
  }

  stop(
    "'x' is not a vector of valid log levels but ", x, ". Valid log levels are: ",
    paste0(log_levels, " (", names(log_levels), ")", collapse = ", ")
  )
}




assert_valid_threshold <- function(x, log_levels = getOption("yog.log_levels"), msg = ""){
  assert(
    !is.na(x) && is_scalar_integerish(x),
    msg,
    "'x' must either the numeric or character representation of one of the following log levels: ",
    paste(sprintf("%s (%s)", names(log_levels), log_levels), collapse = ", ")
  )
}

