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




#' Title
#'
#' @param where
#'
#' @return
#' @export
#'
#' @examples
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



class_fmt <- function(x, ignore = "R6"){
  fmt_class(setdiff(class(x), ignore))
}

fmt_class <- function(x){
  paste0("<", paste(x, collapse = "/"), ">")
}


style_accent <- colt::clt_chr_accent
style_subtle <- colt::clt_chr_subtle




#' Paste and Truncate
#'
#' @param x a vector
#' @param width (maximum) width of result
#' @inheritParams paste
#'
#' @return a `character` scalar
#' @noRd
#'
#' @example
#'   ptrunc(month.abb)
#'   ptrunc(month.abb, month.name)
#'
ptrunc <- function(
  ...,
  width = 40L,
  sep = ", ",
  collapse = ", "
){
  assert(width > 7L, "The minimum supported width is 8")
  x <- paste(..., sep = sep, collapse = collapse)
  width <- width + nchar(x) - crayon::col_nchar(x)

  sel <- vapply(x, nchar, integer(1), USE.NAMES = FALSE) > width

  x[sel] <- strtrim(x[sel], width = width - 4L)
  x[sel] <- paste(gsub(",{0,1}\\s*$", "", x[sel]), "...")
  x
}

