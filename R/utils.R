`%||%` <- function (x, y){
  if (is.null(x))
    y
  else
    x
}



assert_valid_threshold <- function(x, log_levels, msg = ""){
  assert(
    !is.na(x) && is_scalar_integerish(x),
    msg,
    "'x' must either the numeric or character representation of one of the following log levels: ",
    paste(sprintf("%s (%s)", names(log_levels), log_levels), collapse = ", ")
  )
}
