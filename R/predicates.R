is_valid_log_level <- function(x, log_levels = getOption("yog.log_levels")){
  is_scalar(x) &&
  is_valid_log_levels(x, log_levels = log_levels)
}



is_valid_log_levels <- function(
  x,
  log_levels = getOption("yog.log_levels")
){
  is.atomic(x) &&
  (is.na(x) || x %in% names(log_levels) || (is_integerish(x) & x >= 0) )
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




is_valid_threshold <- function(
  x,
  log_levels = getOption("yog.log_levels")
){
  is.na(x) ||
  is_scalar_integerish(x) ||
  (is_scalar_character(x) && x %in% names(log_levels))
}



assert_valid_threshold <- function(
  x,
  log_levels = getOption("yog.log_levels"),
  msg = ""
){
  assert(
    is_valid_threshold(x, log_levels = log_levels),
    msg,
    "'x' must either the numeric or character representation of one of the following log levels: ",
    paste(sprintf("%s (%s)", names(log_levels), log_levels), collapse = ", ")
  )
}
