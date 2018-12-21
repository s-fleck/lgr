#' Information About the System
#'
#' `get_user()` tries to determine the current user. Defaults to
#' `getOption("yog.user")`. If the option is not set, `Sys.info()[["user"]]`
#' is used. If the option is not set and the package **whoami** is available,
#' the user name is guessed based on whichever of the following is available:
#' `email_address`, `fullname`, `gh_username`, `username`.
#'
#'
#' @return a `character` scalar.
#'
#'
#' @seealso [whoami::whoami()]
#'
#' @name system_infos
#' @examples
#' get_user()
#'
#' @param fallback A fallback in case the user name could not be determined
#' @rdname system_infos
#' @export
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



generate_sql_create_table <- function(
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

  assert(all(
    is.na(col_names) == FALSE |
    is.na(col_names) == is.na(col_types)
  ))

  sql_opts[is.na(sql_opts)] <- ""
  col_types  <- toupper(col_types)


  # process input
  empty_cols <- is.na(col_names) && is.na(col_types)
  col_names  <- col_names[!empty_cols]
  col_types  <- col_types[!empty_cols]

  if (any(is.na(col_types))){
    warning(sprintf(
      "Skipping %s columns with col_type 'NA'", sum(is.na(col_types))
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



standardize_log_level <- function(
  x,
  log_levels = getOption("yog.log_levels")
){
  assert(is_scalar(x))
  standardize_log_levels(x, log_levels = log_levels)
}




standardize_log_levels <- function(
  x,
  log_levels = getOption("yog.log_levels")
){
  if (is_integerish(x)){
    assert(all(x > 0))
    return(as.integer(x))
  }

  if (is.character(x)){
    all(x %in% names(log_levels))
    return(log_levels[match(labels, names(log_levels))])
  }

  stop("'x' is not valid log_levels")
}
