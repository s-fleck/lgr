
context("utils")


test_that("utils works as expected", {

  foo <- function() get_caller(-1L)
  expect_identical(foo(), "foo")

  lg <- Logger$new(
    "test logger",
    appenders = AppenderConsole$new(layout = LayoutFormat$new(fmt = "%c")),
    propagate = FALSE
  )


  foobar <- function() lg$error("test")
  expect_output(foobar(), "foobar")

  fizzbuzz <- function() foobar()
  expect_output(fizzbuzz(), "foobar")

  blahblubb <- function() lg$log(200, "test")
  expect_output(blahblubb(), "blahblubb")

  expect_match(get_caller(-99), "shell")
})



test_that("standardize log_levels / threshold works", {
  expect_error(standardize_log_level(NA))
  expect_identical(standardize_threshold(NA), NA_integer_)

  blubb <- 100
  expect_identical(standardize_log_levels(blubb), 100L)
  expect_identical(standardize_log_level(blubb), 100L)
  expect_identical(standardize_threshold(blubb), 100L)

  blubb <- NA
  expect_error(standardize_log_levels(blubb), "blubb.*trace")
  expect_error(standardize_log_level(blubb), "blubb.*trace")
  expect_identical(standardize_threshold(blubb), NA_integer_)

  blubb <- "all"
  expect_error(standardize_log_levels(blubb), "blubb.*trace")
  expect_error(standardize_log_level(blubb), "blubb.*trace")
  expect_identical(standardize_threshold(blubb), NA_integer_)

  blubb <- "off"
  expect_error(standardize_log_levels(blubb), "blubb.*trace")
  expect_error(standardize_log_level(blubb), "blubb.*trace")
  expect_identical(standardize_threshold(blubb), 0L)

  blubb <- 0
  expect_error(standardize_log_levels(blubb), "blubb.*trace")
  expect_error(standardize_log_level(blubb),  "blubb.*trace")
  expect_identical(standardize_threshold(blubb),  0L)


  blubb <- "info"
  expect_identical(standardize_log_levels(blubb), 400L)
  expect_identical(standardize_log_level(blubb),  400L)
  expect_identical(standardize_threshold(blubb),  400L)

  blubb <- c("info", "warn")
  expect_identical(standardize_log_levels(blubb), c(400L, 300L))
  expect_error(standardize_log_level(blubb),  "blubb.*scalar")
  expect_error(standardize_threshold(blubb),  "threshold.*scalar")

  blubb <- c(400, 300)
  expect_identical(standardize_log_levels(blubb), c(400L, 300L))
  expect_error(standardize_log_level(blubb),  "blubb.*scalar")
  expect_error(standardize_threshold(blubb),  "threshold.*scalar")

  blubb <- c(0, 300)
  expect_error(standardize_log_levels(blubb), "blubb.*trace")
  expect_error(standardize_log_level(blubb),  "blubb.*scalar")
  expect_error(standardize_threshold(blubb),  "threshold.*scalar")

  blubb <- "bar"
  expect_error(standardize_log_levels(blubb), "blubb.*trace")
  expect_error(standardize_log_level(blubb), "blubb.*trace")
  expect_error(standardize_threshold(blubb), "blubb.*trace")
})




test_that("get_caller() does something useful for corner cases", {
  foo <- function(arg){get_caller()}
  expect_identical(foo(), "foo")

  r <- {function(arg){get_caller()}}()
  expect_identical(r, "{...}")

  r <- (function(arg){"sgnasdkgshdkghsakdghskdjghsdkag"; get_caller()})()
  expect_length(r, 1)
})




# embedded from tabde
test_that("generate_sql works as expected", {

  cn1 <- LETTERS[1:18]
  ct1 <- c("SMALLINT", "INTEGER", "INT", "BIGINT", "DECIMAL", "NUMERIC", "DECFLOAT",
           "REAL", "DOUBLE", "CHARACTER", "CHARACTER(1)", "VARCHAR(9)", "CLOB(1)",
           "GRAPHIC(80)", "VARGRAPHIC(80)", "DBCLOB(80)", "BLOB(80)", "FAIL")
  co1 <- c(rep("NOT NULL", length(ct1)))

  expect_silent(sql_create_table("testtable", cn1[1:3], ct1[1:3]))
  expect_error(
    sql_create_table("testtable", cn1[1:3], ct1[1:2]),
    "is_equal_length"
  )

  ct1[[1]] <- NA

  expect_message(
    sql_create_table("testtable", cn1[1:3], ct1[1:3]),
    "Skipping 1"
  )

  cn1[[1]] <- NA
  expect_error(
    sql_create_table("testtable", cn1[1:3], ct1[1:3]),
    "must be unique"
  )

  cn1[[1]] <- "B"
  expect_error(
    sql_create_table("testtable", cn1[1:3], ct1[1:3]),
    "must be unique"
  )
})
