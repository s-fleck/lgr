context("utils-logging")




test_that("log suppression", {
  expect_output(FATAL("test"), "FATAL")
  expect_silent(without_logging(FATAL("test")))
  expect_output(FATAL("test"), "FATAL")

  suspend_logging()
  expect_silent(FATAL("test"))
  unsuspend_logging()
  expect_output(FATAL("test"), "FATAL")
})




test_that("without_logging does not conflict with suspend/unsuspend logging", {
  suspend_logging()
  without_logging("blah")
  expect_identical(getOption("lgr.logging_suspended"), TRUE)
  unsuspend_logging()
})




test_that("with_log_level works", {
  lg <- Logger$new("test")

  expect_output(
    with_log_level("warn", lg$info("blubb"), logger = lg),
    "WARN"
  )

  foo <- function(){
    with_log_level("trace", lg$info("blubb"), logger = lg)
  }
  expect_silent(foo())

  expect_identical(lg$last_event$caller, "foo")
})



test_that("with_log_level works", {
  lg <- Logger$new(
    "test",
    appenders = AppenderConsole$new(layout = LayoutJson$new()),
    propagate = FALSE
  )

  expect_output(
    with_log_value(list(foo = "bar", a = 1:2), lg$info("blubb"), logger = lg),
    '"a":\\[1,2\\],"foo":"bar"'
  )

  foo <- function(){
    with_log_value(list(foo = "bar"), lg$info("blubb"), logger = lg)
  }
  expect_output(foo())

  expect_identical(lg$last_event$caller, "foo")
})
