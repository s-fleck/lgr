context("default_functions")


test_that("default_functions works as expected", {
  lg <- get_logger("a/test/logger")
  lg$set_exception_handler(function(e) e)

  x <- lg$info(stop("blahblah"))
  expect_warning(default_exception_handler(x), class = "LoggerWarning")

  x$appender <- AppenderConsole$new()
  expect_warning(default_exception_handler(x), class = "AppenderWarning")
  expect_warning(default_exception_handler(x), class = "LoggerWarning")
})
