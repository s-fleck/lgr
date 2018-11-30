#' Title
#'
#' @param fallback
#'
#' @return
#' @export
#'
#' @examples
get_user <- function(fallback = "unknown user"){
  if (requireNamespace("whoami", quietly = TRUE)){
    res <- try({
      whoami::email_address(
        whoami::fullname(
          whoami::gh_username(
            whoami::username(
              fallback
      ))))
    }, silent = TRUE)
  } else {
    res <- try(Sys.info()[["user"]], silent = TRUE)
  }

  if (inherits(res, "try-error") || is.null(res))
      res <- fallback

  res
}




`%||%` <- function (x, y){
  if (is.null(x))
    y
  else
    x
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
    return(deparse(res))
  }
}




if (requireNamespace("crayon", quietly = TRUE)){
  style_accent <- crayon::silver
  style_subtle <- crayon::silver
  col_nchar <- crayon::col_nchar
} else {
  style_accent <- identity
  style_subtle <- identity
  col_nchar <- nchar
}




#' Paste and Truncate
#'
#' color aware version of ptrunc from sfmisc
#'
ptrunc_col <- function(
  ...,
  width = 40L,
  sep = ", ",
  collapse = ", "
){
  assert(width > 7L, "The minimum supported width is 8")
  x <- paste(..., sep = sep, collapse = collapse)
  width <- width + nchar(x) - col_nchar(x)

  sel <- vapply(x, nchar, integer(1), USE.NAMES = FALSE) > width

  x[sel] <- strtrim(x[sel], width = width - 4L)
  x[sel] <- paste(gsub(",{0,1}\\s*$", "", x[sel]), "...")
  x
}
