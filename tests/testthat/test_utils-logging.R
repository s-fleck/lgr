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




test_that("with_log_level works", {
  lg <- Logger$new("test")

  with_log_level("warn", {
    lg$info("blubb")
  }, logger = lg)
})
