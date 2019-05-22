expect_equal_timestamp = function(x, y){
  assert(is_POSIXct(x))
  assert(is_POSIXct(y))
  attr(x, "tzone") <- NULL
  attr(y, "tzone") <- NULL
  testthat::expect_true(all(
    format(x, usetz = FALSE) == format(y, usetz = FALSE)
  ))
}



expect_setequal_timestamp = function(x, y){
  assert(is_POSIXct(x))
  assert(is_POSIXct(y))
  attr(x, "tzone") <- NULL
  attr(y, "tzone") <- NULL
  testthat::expect_setequal(
    format(x, usetz = FALSE), format(y, usetz = FALSE)
  )
}




dbRemoveTableCaseInsensitive <- function(
  conn,
  name
){
  if (inherits(name, "Id")){
    r0 <- try(DBI::dbRemoveTable(conn, name), silent = TRUE)
    name <- paste0(name@name[["schema"]], ".", name@name[["table"]])
  }

    r1 <- try(DBI::dbRemoveTable(conn, name), silent = TRUE)
    r2 <- try(DBI::dbRemoveTable(conn, toupper(name)), silent = TRUE)
    r3 <- try(DBI::dbRemoveTable(conn, tolower(name)), silent = TRUE)
    res <- !is_try_error(r1) || !is_try_error(r2) || !is_try_error(r3)


  res
}
