context("utils-logging")




test_that("log suppression", {
  lg <- get_logger("test")

  # without_logging() suppresses log messages temporarily
  expect_output(lg$fatal("test"), "FATAL")
  expect_silent(without_logging(lg$fatal("test")))
  expect_output(lg$fatal("test"), "FATAL")

  # suspending works and can be ignored with with_logging()
  suspend_logging()
  expect_silent(lg$fatal("test"))
  expect_output(with_logging(lg$fatal("test")))

  # unsuspending logging works
  unsuspend_logging()
  expect_output(lg$fatal("test"), "FATAL")
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

})
