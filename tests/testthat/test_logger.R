context("yog")


test_that("active bindings", {
  ml <- Logger$new()

  expect_identical(
    ml$log_levels,
    structure(
      setNames(1L:6L, c("fatal", "error", "warn", "info", "debug", "trace")),
      class = c("log_levels", "integer")
    )
  )
  expect_error(ml$log_levels <- ml$log_levels, "cannot be modified")


  expect_silent(ml$threshold <- 5)
  expect_identical(ml$threshold, 5L)
  expect_silent(ml$threshold <- "fatal")
  expect_identical(ml$threshold, 1L)
  expect_error(ml$threshold <- "blubb", "fatal.*trace")


  walk(ml$appenders, function(.x) expect_true(inherits(.x, "Appender")))

  expect_silent(ml$user <- "blubb")
  expect_identical(ml$user, "blubb")
  expect_error(ml$user <- 5, "'user'")

  expect_true(is.function(ml$string_formatter))
})




test_that("basic logging", {
  ml <- Logger$new()
  ts <- structure(1540486764.41946, class = c("POSIXct", "POSIXt"))

  testfun <- function(){
    ml$fatal("blubb")
  }

  expect_output({
    testfun()
    testfun()
  })

})




test_that("suspending loggers works", {

  expect_output(ml_col$fatal("blubb"), "FATAL")
  x <- capture.output(ml_col$fatal("blubb"))
  ml_col$suspend()
  expect_identical(ml_col$fatal("blubb %s", "blah"), NULL)
  ml_col$unsuspend()
  y <- capture.output(ml_col$fatal("blubb"))

  # ignore timestamp for comparison
  x <- gsub("[.*]", x, "[time]")
  y <- gsub("[.*]", x, "[time]")

  expect_identical(x, y)
})
