guess_user <- function(fallback = "unknown user"){
  if (requireNamespace("whoami", quietly = TRUE)){
    res <-
      whoami::email_address(
        whoami::fullname(
          whoami::gh_username(
            whoami::username(
              fallback
    ))))
  } else {
    res <- try(Sys.info()$user)
    if (inherits(res, "try-error") || is.null(res))
      res <- fallback
  }

  res
}




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




get_caller <- function(
  where = -5L
){
  res <- try(sys.call(where)[[1]], silent = TRUE)

  if (is.null(res) || inherits(res, "try-error")){
    return("(shell)")

  } else {
    res <- deparse(res)

    if (grepl("::", res, fixed = TRUE)){
      return(res)

    } else {
      ns <- try(format(findFunction(res)[[1]]), silent = TRUE)

      if (inherits(ns, "try-error")){
        return(res)
      } else if (grepl("namespace", ns)){
          return(paste0(sub(".*namespace:([^>]+)>.*", "\\1", ns), "::", res))
      } else {
        return(paste0(sub(".*environment:([^>]+)>.*", "\\1", ns), "::", res))
      }
    }
  }
}
