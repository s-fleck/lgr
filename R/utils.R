#' Get the Current System User
#'
#' Try to determine the current user. Defaults to `getOption("yog.user")`. If
#' the option is not set, `Sys.info()[["user"]]` is used. If the option is not
#' set and the package **whoami** is available, the user name
#' is guessed to whichever of the following is available: `email_address``,
#' `fullname`, `gh_username`, `username`.
#'
#' @param fallback A fallback in case the user name could not be determined
#'
#' @return
#'   a `character` scalar.
#' @export
#'
#' @examples
#'   get_user()
get_user <- function(fallback = "unknown user"){
  guess_user <- function(){
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


  getOption("yog.user", guess_user())
}




`%||%` <- function (x, y){
  if (is.null(x))
    y
  else
    x
}




#' Get the Calling Function
#'
#' Tries to determine the calling functions based on `where`.
#'
#' @param where `integer` scalar (usually negative). Look up that many frames
#'   up the call stack
#'
#' @return a `character` scalar
#' @seealso [base::sys.call()]
#' @export
#'
#' @examples
#' foo <- function() get_caller(-1L)
#' foo()
#'
#'
get_caller <- function(
  where = -1L
){
  res <- try(sys.call(where)[[1]], silent = TRUE)

  if (is.null(res) || inherits(res, "try-error")){
    return("(shell)")

  } else {
    return(deparse(res))
  }
}




#' Paste and Truncate
#'
#' color aware version of ptrunc from sfmisc
#' @noRd
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



