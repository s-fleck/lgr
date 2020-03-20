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



# embedded from tabde
#' Generate SQL CREATE TABLE statements
#'
#' Creates SQL CREATE TABLE statements from a vector of column names and
#' a vector of column types
#'
#' @param tname `character` scalar. Name of target sql table
#' @param col_names `character` vector. Column names of target sql table
#' @param col_types `character` scalar. Column types of target sql table.
#'   Columns of type `NA` will be skipped
#' @param sql_opts column options of target sql table (for example `NOT NULL`)
#'
#' @return a `CREATE TABLE` statement as a `character` scalar
#' @noRd
#'
#' @examples
#' sql_create_table(
#'   "example.table",
#'   c("numbers", "animals"),
#'   c("integer", "varchar(8)"),
#'   c("NOT NULL", "")
#' )
sql_create_table <- function(
  tname,
  col_names,
  col_types,
  sql_opts = rep("", length(col_names))
){
  # preconditions
  stopifnot(
    is_scalar_character(tname),
    is.character(col_names),
    is.character(col_types),
    is_equal_length(col_names, col_types, sql_opts)
  )

  assert(
    !anyNA(col_names) && all_are_distinct(col_names),
    "All `col_names` must be unique and non-`NA`"
  )

  sql_opts[is.na(sql_opts)] <- ""
  col_types  <- toupper(col_types)

  # process input
  if (anyNA(col_types)){
    message(sprintf(
      "Skipping %s columns where `col_type` equals `NA`", sum(is.na(col_types))
    ))
    col_names <- col_names[!is.na(col_types)]
    col_types <- col_types[!is.na(col_types)]
    sql_opts  <- sql_opts[!is.na(col_types)]
  }

  cols <- paste0(
    trimws(paste0(col_names, " ", col_types, " ", sql_opts)),
    collapse = ", "
  )

  sprintf("CREATE TABLE %s (%s)", tname, cols)
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


isFALSE <- function(x) identical(x, FALSE)
# nocov end
