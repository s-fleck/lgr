#' Information About the System
#'
#' `get_caller()` Tries to determine the calling functions based on `where`.
#'
#' @param where `integer` scalar (usually negative). Look up that many frames
#'   up the call stack
#'
#' @seealso [base::sys.call()]
#' @export
#'
#' @rdname system_infos
#' @export
#'
#' @examples
#' foo <- function() get_caller(-1L)
#' foo()
get_caller <- function(
  where = -1L
){
  res <- tryCatch(
    sys.call(where)[[1]],
    error = function(e) NULL
  )

  if (is.symbol(res)){
    deparse(res)
  } else if (is.null(res)){
    "(shell)"
  } else if (inherits(res, "{")){
    "{...}"
  } else if (is.function(res)){
    # rare, but can happen f.e. through plumber
    fmt_function_signature(res)
  } else {
    ptrunc(deparse(res))
  }
}




#' `get_user()` tries to determine the current user. Defaults to
#' `getOption("lgr.user")`. If the option is not set, `Sys.info()[["user"]]`
#' is used. If the option is not set and the package **whoami** is available,
#' the user name is guessed based on whichever of the following is available:
#' `email_address`, `fullname`, `gh_username`, `username`.
#'
#' @return a `character` scalar.
#'
#' @seealso [whoami::whoami()]
#' @name system_infos
#' @param fallback A fallback in case the user name could not be determined
#' @rdname system_infos
#' @export
#' @examples
#' get_user()

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


  getOption("lgr.user", guess_user())
}




# internal --------------------------------------------------------------

#' Paste and Truncate
#'
#' color aware version of ptrunc from sfmisc
#' @noRd
ptrunc_col <- function(
  ...,
  width = 40L,
  sep = ", ",
  collapse = ", ",
  dots = " ..."
){
  assert(width > 7L, "The minimum supported width is 8")
  x <- paste(..., sep = sep, collapse = collapse)
  width <- width + nchar(x) - col_nchar(x)

  sel <- vapply(x, nchar, integer(1), USE.NAMES = FALSE) > width

  x[sel] <- strtrim(x[sel], width = width - 4L)
  x[sel] <- paste0(gsub(",{0,1}\\s*$", "", x[sel]), dots)
  x
}






# misc --------------------------------------------------------------------

# nocov start
dyn_register_s3_method <- function(
  pkg,
  generic,
  class,
  fun = NULL
){
  stopifnot(
    is_scalar_character(pkg),
    is_scalar_character(generic),
    is_scalar_character(class)
  )

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}




last <- function(x){
  x[length(x)]
}




is_Id = function(x){
  inherits(x, "Id")
}





is_zipcmd_available <- function(cmd = Sys.getenv("R_ZIPCMD", "zip")){

  if (is_blank(cmd)){
    return(FALSE)
  }

  if (.Platform$OS.type == "windows"){
    suppressWarnings(res <- system2("where", cmd, stderr = NULL, stdout = NULL))
  } else {
    res <- tryCatch(
      system2("command", paste("-v", cmd), stderr = NULL, stdout = NULL),
      warning = function(w) {99}
    )
  }

  assert(is_scalar(res))
  res == 0
}




fmt_bytes <- function(
  x
){
  x <- as.numeric(x)

  readablifiy <- function(.x){
    for (unit in c("B", "KiB", "MiB", "GiB", "TiB")){
      if (max(abs(.x)) < 1024 || unit == "TiB")
        break
      else
        .x <- .x / 1024
    }
    return(paste(round(.x, 1), unit))
  }

  vapply(x, readablifiy, character(1))
}



#' Logger Error Conditions
#'
#' @param class `character` scalar. The abstract class that was mistakenly
#'   tried to initialize. The default is to discover the class name
#'   automatically if called inside `$initialize(){...}` in an [R6::R6] class
#'   definition
#'
#' @return a condition object
#' @export
#' @family developer tools
CannotInitializeAbstractClassError <- function(
  class = parent.frame(2)[["classes"]]
){
  error(
    paste(fmt_class(class), "is an abstract class and cannot be initlized"),
    class = c("CannotInitializeAbstractClassError", "NotImplementedError"),
    call = NULL
  )
}




# stricter & faster thant base R version & support for R < 3.5
isFALSE <- function(x){
  identical(x, FALSE)
}



# like utils::tail.default, but saves you from importing utils.
last_n <- function(x, n){
  assert(
    is_n0(n),
    "`n` must be a postive integer >= 0, not ", string_repr(n)
  )

  len <- length(x)
  n   <- min(n, len)
  x[seq.int(to = len, length.out = n)]
}




# conditions --------------------------------------------------------------

condition <- function(message, class, call = NULL, ...) {
  structure(
    class = union(class, "condition"),
    list(message = message, call = call, ...)
  )
}




error <- function(message, class = NULL, call = NULL, ...) {
  structure(
    class = union(class, c("error", "condition")),
    list(message = message, call = call, ...)
  )
}




warning_condition <- function(message, class, call = NULL, ...) {
  structure(
    class = union(class, c("warning", "condition")),
    list(message = message, call = call, ...)
  )
}
# nocov end
